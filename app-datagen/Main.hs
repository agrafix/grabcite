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
import GrabCite.DB
import GrabCite.Dblp
import GrabCite.GetCitations
import GrabCite.GlobalId
import Util.Sentence

import Control.Applicative
import Control.Concurrent.Async
import Control.Exception
import Control.Logger.Simple
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Conduit
import Data.List
import Data.Maybe
import Data.Time.TimeSpan
import Options.Generic
import Path
import Path.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Traversable as T
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
    , c_outDir :: w ::: Maybe FilePath <?> "Directory to write the output to"
    , c_outDb :: w ::: Maybe String <?> "PostgreSQL database to sync with (connection string)"
    , c_dbMigDir :: w ::: Maybe FilePath <?> "Directory holding database migration scripts"
    , c_dbOverwrite :: w ::: Bool <?> "Should existing content be overwritten?"
    , c_dataSourceName :: w ::: Maybe String <?> "What's the name of the data set"
    , c_jobs :: w ::: Int <?> "Number of concurrent tasks to run"
    , c_debug :: w ::: Bool <?> "Enable debug output"
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

data OutDbCfg
    = OutDbCfg
    { odc_connStr :: !String
    , odc_migDir :: !FilePath
    , odc_overwrite :: !Bool
    , odc_srcName :: !String
    }

data OutDb
    = OutDb
    { od_store :: !Store
    , od_overwrite :: !Bool
    , od_src :: !String
    }

data OutMode
    = OutMode
    { om_dir :: !(Maybe (Path Rel Dir))
    , om_db :: !(Maybe OutDb)
    }

main :: IO ()
main =
    withGlobalLogging (LogConfig Nothing True) $
    do x <- unwrapRecord "GrabCite DataGen"
       let cfg :: Config Unwrapped
           cfg = x
       setLogLevel (if c_debug cfg then LogDebug else LogInfo)
       inDir <- handleDir (c_inDir cfg)
       outDir <-
           flip T.mapM (c_outDir cfg) $ \(odx :: FilePath) ->
           do od <- handleDir odx
              createDirIfMissing True od
              pure od
       outDb <-
           flip T.mapM (c_outDb cfg) $ \odb ->
           do let oc :: Maybe OutDbCfg
                  oc =
                      OutDbCfg
                      <$> pure odb
                      <*> c_dbMigDir cfg
                      <*> pure (c_dbOverwrite cfg)
                      <*> c_dataSourceName cfg
              case oc of
                Nothing ->
                    fail "Missing some database params"
                Just ok -> pure ok
       case (c_arxivToTexMode cfg, c_arxivMetaXml cfg, outDir) of
         (True, Just fp, Just od) ->
             do fp' <- handleFile fp
                runArxivConv fp' inDir od
         (True, Nothing, _) ->
             fail "Missing --arxiv-meta-xml argument"
         (True, _, Nothing) ->
             fail "Missing --out-dir argument"
         _ ->
             do let withStore go =
                        case outDb of
                          Nothing -> go Nothing
                          Just y ->
                              do st <- newPgSqlStore (odc_migDir y) (BSC.pack $ odc_connStr y)
                                 go (Just $ OutDb st (odc_overwrite y) (odc_srcName y))
                withStore $ \s -> runDataGen cfg inDir (OutMode outDir s)

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

runDataGen :: Config Unwrapped -> Path Rel Dir -> OutMode -> IO ()
runDataGen cfg inDir outMode =
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
       state <- fromMaybe initState <$> T.mapM loadState (om_dir outMode)
       let todoPdfs = filter (not . flip S.member (s_completed state)) $ F.toList out
       logNote $ "Unhandled files: " <> showText (length todoPdfs)
       let cacheMaker =
               case om_dir outMode of
                 Just d -> withRefCache (d </> [relfile|ref_cache.json|])
                 Nothing -> withMemRefCache
       cacheMaker $ \rc ->
           workLoop mode (c_jobs cfg) outMode state
           todoPdfs (Cfg rc)

sentenceSplitter :: T.Text -> T.Text
sentenceSplitter = T.intercalate "\n============\n" . sentenceSplit

workLoop :: InMode -> Int -> OutMode -> State -> [Path Abs File] -> Cfg -> IO ()
workLoop mode jobs outMode st todoQueue rc =
    do let upNext = take jobs todoQueue
           todoQueue' = drop jobs todoQueue
       cmarkers <-
           fmap (catMaybes . catMaybes) $
           forConcurrently upNext $ \f ->
           timeoutTS (minutes 2) $
           do cm <-
                  try $! handlePdf mode rc f
              case cm of
                Left (ex :: SomeException) ->
                    do logError ("Failed to work on " <> showText f <> ": " <> showText ex)
                       pure Nothing
                Right (paperMeta, extractRes, full, refTable) ->
                    do fw <-
                         case om_dir outMode of
                         Just outDir ->
                             do fbase <-
                                    parseRelFile $ FP.dropExtension $ toFilePath $ filename f
                                fullWriter <-
                                    async $
                                    do let fullFile = toFilePath (outDir </> fbase) <> ".txt"
                                       T.writeFile fullFile full
                                       let refsFile = toFilePath (outDir </> fbase) <> ".refs"
                                       T.writeFile refsFile (refTableText refTable)
                                       case paperMeta of
                                         Nothing -> logInfo "No paper info"
                                         Just pp ->
                                             do let metaFile = toFilePath (outDir </> fbase) <> ".meta"
                                                logInfo ("Wrote meta file to " <> showText metaFile)
                                                BSL.writeFile metaFile (encode pp)
                                       logInfo ("Wrote full text to " <> showText fullFile)
                                wait fullWriter
                                pure $ Just f
                         Nothing -> pure Nothing
                       fw2 <-
                           case extractRes of
                             Just r ->
                                 do out <-
                                        try $!
                                        flip F.mapM_ (om_db outMode) $ \outDb ->
                                        handleExtractionResult (od_store outDb) (od_overwrite outDb)
                                            (ExtractionSource $ T.pack (od_src outDb)) r
                                    case out of
                                      Left (ex :: SomeException) ->
                                          do logError ("Failed to write " <> showText f <> " to db: " <> showText ex)
                                             pure Nothing
                                      Right _ -> pure (Just f)
                             Nothing ->
                                 pure Nothing
                       pure (fw <|> fw2)
       let st' = foldl' updateState st cmarkers
       F.mapM_ (\d -> writeState d st') (om_dir outMode)
       if not (null todoQueue')
          then workLoop mode jobs outMode st' todoQueue' rc
          else do logInfo "All done"
                  pure ()

refTableText :: HM.HashMap GlobalId T.Text -> T.Text
refTableText tbl =
    T.unlines $ map go $ HM.toList tbl
    where
      go (gid, line) =
          textGlobalId gid <> ";" <> T.strip (T.replace ";" "," (T.replace "\n" " " line)) <> ";"

updateState :: State -> Path Abs File -> State
updateState st f =
    st
    { s_completed = S.insert f (s_completed st)
    }

data PaperMeta
    = PaperMeta
    { pm_title :: !T.Text
    , pm_authors :: ![T.Text]
    , pm_url :: !(Maybe T.Text)
    } deriving (Show, Eq)

instance ToJSON PaperMeta where
    toJSON pm =
        object
        [ "title" .= pm_title pm
        , "authors" .= pm_authors pm
        , "url" .= pm_url pm
        ]

exResToMeta :: ExtractionResult (Maybe DblpPaper) -> Maybe PaperMeta
exResToMeta er =
    case er_paperId er of
      Just dp ->
          Just PaperMeta
          { pm_title = dp_title dp
          , pm_authors = dp_authors dp
          , pm_url = db_url dp
          }
      Nothing ->
          case er_titleLine er of
            Just ln ->
                Just PaperMeta
                { pm_title = ln
                , pm_authors = []
                , pm_url = Nothing
                }
            Nothing ->
                Nothing

handlePdf ::
    InMode -> Cfg -> Path Abs File
    -> IO ( Maybe PaperMeta
          , Maybe (ExtractionResult (Maybe DblpPaper))
          , T.Text, HM.HashMap GlobalId T.Text
          )
handlePdf mode rc fp =
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
                pure (Nothing, Nothing, "", mempty)
         Just er ->
             let nodes = globalCitId <$> er_nodes er
                 uniqueRefs =
                     let refNodes = mapMaybe getRefNode nodes
                         go mp rn =
                             HM.insert (cr_tag rn) (cr_info rn) mp
                     in foldl' go mempty refNodes
             in pure
                  ( exResToMeta er
                  , Just er
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
    } deriving (Show, Eq, Generic)

initState :: State
initState =
    State
    { s_completed = S.empty
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
