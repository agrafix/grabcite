{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List
import GrabCite
import GrabCite.Context
import GrabCite.GetCitations

import Control.Concurrent.Async
import Control.Exception
import Control.Logger.Simple
import Data.Aeson
import Data.Maybe
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
import qualified System.FilePath as FP

data Config w
    = Config
    { c_inDir :: w ::: FilePath <?> "Directory holding the paper PDFs"
    , c_recursive :: w ::: Bool <?> "Should we recursively look for papers in input dir?"
    , c_outDir :: w ::: FilePath <?> "Directory to write the output to"
    , c_jobs :: w ::: Int <?> "Number of concurrent tasks to run"
    , c_context :: w ::: Int <?> "Number of words before and after to consider as context"
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
                          filter (\f -> fileExtension f == ".pdf") files
                  pure pdfFiles
       (out :: Seq.Seq (Path Abs File)) <-
           walkDirAccum descender outwriter inDir
       logInfo $ "Found " <> showText (length out) <> " pdfs in " <> showText inDir
       state <- loadState outDir
       let todoPdfs = filter (not . flip S.member (s_completed state)) $ F.toList out
       logNote $ "Unhandled pdfs: " <> showText (length todoPdfs)
       workLoop (c_context cfg) (c_jobs cfg) outDir state todoPdfs

workLoop :: Int -> Int -> Path Rel Dir -> State -> [Path Abs File] -> IO ()
workLoop ctxWords jobs outDir st todoQueue =
    do let upNext = take jobs todoQueue
           todoQueue' = drop jobs todoQueue
       cmarkers <-
           fmap catMaybes $
           forConcurrently upNext $ \f ->
           do cm <-
                  try $! handlePdf ctxWords f
              case cm of
                Left (ex :: SomeException) ->
                    do logError ("Failed to work on " <> showText f <> ": " <> showText ex)
                       pure Nothing
                Right ok ->
                    do forConcurrently_ (zip ok [1..]) $ \(mc, idx :: Int) ->
                           do let fbase = T.pack $ FP.dropExtension $ toFilePath $ filename f
                              fileName <-
                                  parseRelFile . T.unpack $
                                  fbase <> "_" <> mc_dblpId mc <> "_" <> showText idx <> ".txt"
                              let fpTarget = outDir </> fileName
                              T.writeFile (toFilePath fpTarget) $
                                  mc_before mc
                                  <> " "
                                  <> "[DBLP:" <> mc_dblpId mc <> "]"
                                  <> " "
                                  <> mc_after mc
                       pure $ Just (f, ok)
       let st' = foldl' updateState st cmarkers
       writeState outDir st'
       if not (null todoQueue')
          then workLoop ctxWords jobs outDir st' todoQueue'
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
               { s_refCount = HM.insertWith (+) (mc_dblpId c) 1 (s_refCount s)
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

handlePdf :: Int -> Path Abs File -> IO [ContextedMarker]
handlePdf ctxWords fp =
    do cits <- getCitationsFromPdf fp
       case cits of
         Nothing ->
             do logError ("Failed to get citations from " <> showText fp)
                pure []
         Just er ->
             pure $ getContextedMarkers ctxWords (er_nodes er)

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
