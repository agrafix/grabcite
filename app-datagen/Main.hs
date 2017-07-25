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
import GrabCite.Context
import GrabCite.GetCitations
import GrabCite.GlobalId
import Util.Regex

import Control.Concurrent.Async
import Control.Exception
import Control.Logger.Simple
import Control.Monad
import Data.Aeson
import Data.Char
import Data.List
import Data.Maybe
import Options.Generic
import Path
import Path.IO
import Text.Regex.PCRE.Heavy
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
    { c_inDir :: w ::: FilePath <?> "Directory holding the paper PDFs"
    , c_recursive :: w ::: Bool <?> "Should we recursively look for papers in input dir?"
    , c_textMode :: w ::: Bool <?> "Is the directory already holding text files?"
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
       let descender =
               if c_recursive cfg
               then Just (\_ _ __ -> pure $ WalkExclude [])
               else Just (\_ _ _ -> pure WalkFinish)
           outwriter _ _ files =
               do let pdfFiles =
                          Seq.fromList $
                          filter (\f -> fileExtension f == if c_textMode cfg then ".txt" else ".pdf") files
                  pure pdfFiles
       (out :: Seq.Seq (Path Abs File)) <-
           walkDirAccum descender outwriter inDir
       logInfo $ "Found " <> showText (length out) <> " pdfs in " <> showText inDir
       state <- loadState outDir
       let todoPdfs = filter (not . flip S.member (s_completed state)) $ F.toList out
       logNote $ "Unhandled pdfs: " <> showText (length todoPdfs)
       withRefCache (outDir </> [relfile|ref_cache.json|]) $ \rc ->
           workLoop (c_textMode cfg) (c_writeContexts cfg) (c_context cfg) (c_jobs cfg) outDir state
           todoPdfs (Cfg rc id)

sSplit :: (Char -> (T.Text, T.Text) -> Bool) -> T.Text -> [T.Text]
sSplit decisionFun txtIn =
    filter (not . T.null) $ map (TL.toStrict . TL.strip) $ reverse $ go [] mempty txtIn
    where
      go output current state =
          case T.uncons state of
            Nothing -> (TLB.toLazyText current : output)
            Just (c, more)
                | c == '.' || c == '?' || c == '!' ->
                      let curr = TLB.toLazyText current
                          currL = TL.length curr
                          toDrop = currL - 20
                          alreadyRead = TL.toStrict $ TL.drop (min 0 toDrop) curr
                          afterDot = T.take 20 more
                          shouldSplit = decisionFun c (alreadyRead, afterDot)
                      in if shouldSplit
                            then go (curr <> TL.singleton c : output) mempty more
                            else go output (current <> TLB.singleton c) more
                | otherwise -> go output (current <> TLB.singleton c) more

sentenceSplitter :: T.Text -> T.Text
sentenceSplitter txtIn =
    T.intercalate "\n============\n" $ mapMaybe cleanSentence $
    filter isValidSentence $ sSplit sentenceDecide txtIn

cleanSentence :: T.Text -> Maybe T.Text
cleanSentence tx =
    let tLines = filter goodLine . T.lines $ tx
        lineCount = length tLines
    in if lineCount == 0 || lineCount > 4
       then Nothing
       else Just (gsub multiSpace (T.singleton ' ') $ T.intercalate " " tLines)
    where
      goodLine x =
          not (T.null x) && not (x =~ sectionTitleLine)

isValidSentence :: T.Text -> Bool
isValidSentence txt
    | T.any (\c -> c =='\f' || c == '^' || c == '@') txt = False
    | otherwise = True

sentenceDecide :: Char -> (T.Text, T.Text) -> Bool
sentenceDecide c (before, after)
    | c == '!' = True
    | c == '?' = True
    | localDots beforeRev || localDots after = False
    | noSpace beforeRev && noSpace after = False
    | noSpace beforeRev && beginsCapital after = True
    | isAbbrev beforeRev = False
    | otherwise = True
    where
      beginsCapital =
          T.all isUpper . T.take 1 . T.strip
      noSpace =
          T.all (not . isSpace) . T.take 1
      isAbbrev x =
          let l = T.takeWhile (not . isSpace) $ T.strip x
          in T.length l <= 3 || (T.all isUpper l && T.length l < 5)
      localDots =
          T.any (\cx -> cx =='.' || cx == ',') . T.take 3
      beforeRev = T.reverse before

workLoop :: Bool -> Bool -> Int -> Int -> Path Rel Dir -> State -> [Path Abs File] -> Cfg -> IO ()
workLoop textMode ctxWrite ctxWords jobs outDir st todoQueue rc =
    do let upNext = take jobs todoQueue
           todoQueue' = drop jobs todoQueue
       cmarkers <-
           fmap catMaybes $
           forConcurrently upNext $ \f ->
           do cm <-
                  try $! handlePdf textMode rc ctxWords f
              case cm of
                Left (ex :: SomeException) ->
                    do logError ("Failed to work on " <> showText f <> ": " <> showText ex)
                       pure Nothing
                Right (ok, full) ->
                    do fbase <-
                           parseRelFile $ FP.dropExtension $ toFilePath $ filename f
                       fullWriter <-
                           async $
                           do let fullFile = toFilePath (outDir </> fbase) <> ".txt"
                              T.writeFile fullFile full
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
          then workLoop textMode ctxWrite ctxWords jobs outDir st' todoQueue' rc
          else do logInfo "All done"
                  pure ()


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

handlePdf :: Bool -> Cfg -> Int -> Path Abs File -> IO ([ContextedMarker], T.Text)
handlePdf textMode rc ctxWords fp =
    do cits <-
           if textMode
           then Just <$> getCitationsFromTextFile rc fp
           else getCitationsFromPdf rc fp
       case cits of
         Nothing ->
             do logError ("Failed to get citations from " <> showText fp)
                pure ([], "")
         Just er ->
             let nodes = fmap globalCitId $ er_nodes er
             in pure
                  ( getContextedMarkers ctxWords nodes
                  , sentenceSplitter $ mkTextBody nodes
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

sectionTitleLine :: Regex
sectionTitleLine =
    [reM|^([0-9]+(\.[0-9]+(\.[0-9]+(\.[0-9]+)?)?)?\s+)?[A-Z &\-,\.]+$|]

multiSpace :: Regex
multiSpace =
    [re|\s\s+|]
