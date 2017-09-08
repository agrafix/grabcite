{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import GrabCite
import GrabCite.Annotate
import GrabCite.Arxiv
import GrabCite.Context
import GrabCite.Dblp
import GrabCite.GetCitations
import GrabCite.GlobalId
import Util.Sentence
import qualified Data.ByteString as BS

import Control.Concurrent.Async
import Control.Exception
import Control.Logger.Simple
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Conduit
import Data.List
import Data.Maybe
import Data.Time.TimeSpan
import Options.Generic
import Path
import Path.IO
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified System.FilePath as FP

data Config w
    = Config
    { c_inDir :: w ::: FilePath <?> "Directory holding the input files"
    , c_recursive :: w ::: Bool <?> "Should we recursively look for papers in input dir?"
    , c_pdfMode :: w ::: Bool <?> "PDF input files"
    , c_grobidMode :: w ::: Bool <?> "Grobid Tei XML input files"
    , c_textMode :: w ::: Bool <?> "Is the directory already holding text files?"
    , c_iceCiteMode :: w ::: Bool <?> "Is the directory holding ice-cite files?"
    , c_iceCiteBasicMode :: w ::: Bool <?> "Like ice-cite, but ignore most roles."
    , c_texMode :: w ::: Bool <?> "Input directory is holding tex/bbl files."
    , c_arxivToTexMode :: w ::: Bool <?> "Convert an arxiv dump to a tex/bbl directory"
    , c_arxivMetaXml :: w ::: Maybe FilePath <?> "Arxiv meta xml location"
    , c_outDir :: w ::: FilePath <?> "Directory to write the output to"
    , c_jobs :: w ::: Int <?> "Number of concurrent tasks to run"
    , c_context :: w ::: Int <?> "Number of words before and after to consider as context (context mode)"
    , c_debug :: w ::: Bool <?> "Enable debug output"
    , c_writeContexts :: w ::: Bool <?> "Should context files be written"
    } deriving (Generic)

instance ParseRecord (Config Wrapped) where
    parseRecord = parseRecordWithModifiers modifiers

deriving instance Show (Config Unwrapped)

modifiers :: Modifiers
modifiers =
    lispCaseModifiers
    { fieldNameModifier = fieldNameModifier lispCaseModifiers . drop 2
    }

data InMode
    = InText
    | InPdf
    | InIceCite
    | InIceCiteBasic
    | InTex
    | InGrobid

main :: IO ()
main =
    withGlobalLogging (LogConfig Nothing True) $
    do x <- unwrapRecord "GrabCite DataGen"
       let cfg :: Config Unwrapped
           cfg = x
       setLogLevel (if c_debug cfg then LogDebug else LogInfo)
       inDir <- handleDir (c_inDir cfg)
       outDir <- handleDir (c_outDir cfg)
       createDirIfMissing True outDir
       case (c_arxivToTexMode cfg, c_arxivMetaXml cfg) of
         (True, Just fp) ->
             do fp' <- handleFile fp
                runArxivConv fp' inDir outDir
         (True, Nothing) ->
             fail "Missing --arxiv-meta-xml argument"
         _ -> runDataGen cfg inDir outDir

runArxivConv :: Path Rel File -> Path Rel Dir -> Path Rel Dir -> IO ()
runArxivConv metaXml inDir outDir =
    do let cfg =
               ArxivCfg
               { ac_metaXml = metaXml
               , ac_srcFileDir = inDir
               , ac_desiredSpec = "cs"
               }
       runResourceT $
           arxivSpecLoadingPipeline cfg $$ arxivSpecCopySink outDir

runDataGen :: Config Unwrapped -> Path Rel Dir -> Path Rel Dir -> IO ()
runDataGen cfg inDir outDir =
    do mode <-
           case (c_pdfMode cfg, c_textMode cfg, c_iceCiteMode cfg, c_iceCiteBasicMode cfg, c_texMode cfg, c_grobidMode cfg) of
             (True, False, False, False, False, False) -> pure InPdf
             (False, True, False, False, False, False) -> pure InText
             (False, False, True, False, False, False) -> pure InIceCite
             (False, False, False, True, False, False) -> pure InIceCiteBasic
             (False, False, False, False, True, False) -> pure InTex
             (False, False, False, False, False, True) -> pure InGrobid
             _ -> fail "Can only use one mode at a time!"
       let descender =
               if c_recursive cfg
               then Just (\_ _ __ -> pure $ WalkExclude [])
               else Just (\_ _ _ -> pure WalkFinish)
           outwriter _ _ files =
               do let pdfFiles =
                          Seq.fromList $
                          flip filter files $
                          let ext =
                                  case mode of
                                    InText -> ".txt"
                                    InPdf -> ".pdf"
                                    InIceCite -> ".json"
                                    InIceCiteBasic -> ".json"
                                    InTex -> ".tex"
                                    InGrobid -> ".xml"
                          in \f -> fileExtension f == ext
                  pure pdfFiles
       (out :: Seq.Seq (Path Abs File)) <-
           walkDirAccum descender outwriter inDir
       logInfo $ "Found " <> showText (length out) <> " in files in " <> showText inDir
       state <- loadState outDir
       let todoPdfs = filter (not . flip S.member (s_completed state)) $ F.toList out
       logNote $ "Unhandled files: " <> showText (length todoPdfs)
       withRefCache (outDir </> [relfile|ref_cache.json|]) $ \rc ->
           workLoop mode (c_writeContexts cfg) (c_context cfg) (c_jobs cfg) outDir state
           todoPdfs (Cfg rc)

sentenceSplitter :: T.Text -> T.Text
sentenceSplitter = T.intercalate "\n============\n" . sentenceSplit

workLoop :: InMode -> Bool -> Int -> Int -> Path Rel Dir -> State -> [Path Abs File] -> Cfg -> IO ()
workLoop mode ctxWrite ctxWords jobs outDir st todoQueue rc =
    do let upNext = take jobs todoQueue
           todoQueue' = drop jobs todoQueue
       cmarkers <-
           fmap (catMaybes . catMaybes) $
           forConcurrently upNext $ \f ->
           timeoutTS (minutes 2) $
           do cm <-
                  try $! handlePdf mode rc ctxWords f
              case cm of
                Left (ex :: SomeException) ->
                    do logError ("Failed to work on " <> showText f <> ": " <> showText ex)
                       pure Nothing
                Right (paperId, ok, full, refTable) ->
                    do fbase <-
                           parseRelFile $ FP.dropExtension $ toFilePath $ filename f
                       fullWriter <-
                           async $
                           do let fullFile = toFilePath (outDir </> fbase) <> ".txt"
                              T.writeFile fullFile full
                              let refsFile = toFilePath (outDir </> fbase) <> ".refs"
                              T.writeFile refsFile (refTableText refTable)
                              case paperId of
                                Nothing -> logInfo "No paper info"
                                Just pp ->
                                    do let metaFile = toFilePath (outDir </> fbase) <> ".meta"
                                       logInfo ("Wrote meta file to " <> showText metaFile)
                                       BSL.writeFile metaFile (encode pp)
                              logInfo ("Wrote full text to " <> showText fullFile)
                       when ctxWrite $
                           forConcurrently_ (zip ok [1..]) $ \(mc, idx :: Int) ->
                           do let fpTarget =
                                      toFilePath (outDir </> fbase)
                                      <> "_" <> T.unpack (textGlobalId (mc_id mc))
                                      <> "_" <> show idx <> ".txt"
                              T.writeFile fpTarget $
                                  mc_before mc
                                  <> " "
                                  <> "[" <> textGlobalId (mc_id mc) <> "]"
                                  <> " "
                                  <> mc_after mc
                       wait fullWriter
                       pure $ Just (f, ok)
       let st' = foldl' updateState st cmarkers
       writeState outDir st'
       if not (null todoQueue')
          then workLoop mode ctxWrite ctxWords jobs outDir st' todoQueue' rc
          else do logInfo "All done"
                  pure ()

refTableText :: HM.HashMap GlobalId T.Text -> T.Text
refTableText tbl =
    T.unlines $ map go $ HM.toList tbl
    where
      go (gid, line) =
          textGlobalId gid <> ";" <> line <> ";"

updateState :: State -> (Path Abs File, [ContextedMarker]) -> State
updateState st (f, cxm) =
    let baseSt =
            st
            { s_completed = S.insert f (s_completed st)
            }
        updateCxm s c =
            let wordPrior =
                    listToMaybe $ reverse $ T.words (mc_before c)
                wordAfter =
                    listToMaybe $ T.words (mc_after c)
            in s
               { s_refCount = HM.insertWith (+) (textGlobalId (mc_id c)) 1 (s_refCount s)
               , s_wordPrior =
                       case wordPrior of
                         Just wp -> HM.insertWith (+) wp 1 (s_wordPrior s)
                         Nothing -> s_wordPrior s
               , s_wordAfter =
                       case wordAfter of
                         Just wp -> HM.insertWith (+) wp 1 (s_wordAfter s)
                         Nothing -> s_wordAfter s
               }
    in foldl' updateCxm baseSt cxm

handlePdf ::
    InMode -> Cfg -> Int -> Path Abs File
    -> IO (Maybe DblpPaper, [ContextedMarker], T.Text, HM.HashMap GlobalId T.Text)
handlePdf mode rc ctxWords fp =
    do cits <-
           case mode of
             InText -> Just <$> getCitationsFromTextFile rc fp
             InPdf -> getCitationsFromPdf rc fp
             InGrobid -> BS.readFile (toFilePath fp) >>= getCitationsFromGrobidXml rc
             InIceCite -> BS.readFile (toFilePath fp) >>= getCitationsFromIceCiteJson rc
             InIceCiteBasic ->
                 BS.readFile (toFilePath fp)
                 >>= getCitationsFromIceCiteBasicJson rc
             InTex ->
                 do bblFile <- setFileExtension "bbl" fp
                    x <- BS.readFile (toFilePath fp)
                    hasBbl <- doesFileExist bblFile
                    y <-
                        if hasBbl
                        then Just <$> BS.readFile (toFilePath bblFile)
                        else pure Nothing
                    getCitationsFromTex rc x y
       case cits of
         Nothing ->
             do logError ("Failed to get citations from " <> showText fp)
                pure (Nothing, [], "", mempty)
         Just er ->
             let nodes = globalCitId <$> er_nodes er
                 uniqueRefs =
                     let refNodes = mapMaybe getRefNode nodes
                         go mp rn =
                             HM.insert (cr_tag rn) (cr_info rn) mp
                     in foldl' go mempty refNodes
             in pure
                  ( er_paperId er
                  , getContextedMarkers ctxWords nodes
                  , sentenceSplitter $ mkTextBody nodes
                  , uniqueRefs
                  )

mkTextBody :: [ContentNode GlobalId] -> T.Text
mkTextBody nds =
    TL.toStrict $ TLB.toLazyText $ loop nds mempty
    where
      loop nodes accum =
          case nodes of
            [] -> accum
            (node : more) ->
                case node of
                  CnText txt -> loop more (accum <> TLB.fromText txt)
                  CnRef ref ->
                      let refMarker =
                              " <" <> textGlobalId (cr_tag ref) <> "> "
                      in loop more (accum <> TLB.fromText refMarker)

handleFile :: FilePath -> IO (Path Rel File)
handleFile fp
    | FP.isAbsolute fp = parseAbsFile fp >>= makeRelativeToCurrentDir
    | otherwise = parseRelFile fp

handleDir :: FilePath -> IO (Path Rel Dir)
handleDir fp
    | FP.isAbsolute fp = parseAbsDir fp >>= makeRelativeToCurrentDir
    | otherwise = parseRelDir fp

data State
    = State
    { s_completed :: !(S.Set (Path Abs File))
    , s_refCount :: !(HM.HashMap T.Text Int)
    , s_wordPrior :: !(HM.HashMap T.Text Int)
    , s_wordAfter :: !(HM.HashMap T.Text Int)
    } deriving (Show, Eq, Generic)

initState :: State
initState =
    State
    { s_completed = S.empty
    , s_refCount = HM.empty
    , s_wordPrior = HM.empty
    , s_wordAfter = HM.empty
    }

instance ToJSON State
instance FromJSON State

mkStateFile :: Path t Dir -> Path t File
mkStateFile outDir = outDir </> $(mkRelFile ".state")

loadState :: Path t Dir -> IO State
loadState outDir =
    do let fp = mkStateFile outDir
       isThere <- doesFileExist fp
       if isThere
          then do logInfo ("Found state file at " <> showText fp)
                  bsl <- BSL.readFile (toFilePath fp)
                  case eitherDecode' bsl of
                    Left errMsg -> logFail ("Bad state file: " <> showText errMsg)
                    Right ok -> pure ok
          else do logInfo "No state file found"
                  pure initState

writeState :: Path t Dir -> State -> IO ()
writeState outDir st =
    do let fp = toFilePath (mkStateFile outDir)
       logDebug $ "Writing current state to " <> showText fp
       BSL.writeFile fp (encode st)
