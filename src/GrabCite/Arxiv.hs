{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module GrabCite.Arxiv
    ( arxivSpecLoadingPipeline
    , arxivSpecCopySink
    , ArxivCfg(..), ArxivSource(..)
      -- * Internals for testing
    , parseMetaXml, MetaHeader(..)
    )
where

import Control.Error
import Control.Logger.Simple
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Bifunctor
import Data.Conduit
import Data.List
import Data.List (find)
import Data.Ord
import Data.XML.Types
import Path
import Path.IO
import System.Exit
import System.Process.Typed
import Text.XML.Stream.Parse
import qualified Data.ByteString as BS
import qualified Data.Conduit.List as CL
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Traversable as T
import qualified System.Posix.Files as F

data ArxivCfg a
    = ArxivCfg
    { ac_metaXml :: !(Path a File)
    , ac_srcFileDir :: !(Path a Dir)
    , ac_desiredSpec :: !T.Text
    }

arxivSpecLoadingPipeline ::
    forall a m. (MonadResource m ,MonadMask m, MonadIO m) => ArxivCfg a -> Source m ArxivSource
arxivSpecLoadingPipeline cc =
    parseMetaXml (ac_metaXml cc)
    =$= CL.filter (\mh -> mh_setSpec mh == ac_desiredSpec cc)
    =$= CL.mapMaybeM go
    where
      go :: MetaHeader -> m (Maybe ArxivSource)
      go x =
          (handleSourceForMeta (ac_srcFileDir cc) x) `catchAll` \errMsg ->
          do logError ("Failed to handle " <> mh_ident x <> ": " <> showText errMsg)
             pure Nothing

arxivSpecCopySink :: MonadIO m => Path a Dir -> Sink ArxivSource m ()
arxivSpecCopySink targetDir =
    do next <- await
       case next of
         Nothing -> pure ()
         Just asrc ->
             do let fileNames =
                        do baseFile <- parseRelFile (T.unpack $ as_ident asrc)
                           let baseName = targetDir </> baseFile
                           (,) <$> (baseName <.> "tex") <*> (baseName <.> "bbl")
                case fileNames of
                  Left errMsg ->
                      do logError ("Failed to make filenames for "
                                   <> as_ident asrc <> " : " <> showText errMsg)
                         arxivSpecCopySink targetDir
                  Right (texFile, bblFile) ->
                      do liftIO $
                             do logInfo ("Writing " <> showText texFile)
                                BS.writeFile (toFilePath texFile) (as_tex asrc)
                                F.traverse_ (BS.writeFile $ toFilePath bblFile) (as_bbl asrc)
                         arxivSpecCopySink targetDir

data MetaHeader
    = MetaHeader
    { mh_ident :: !T.Text
    , mh_datestamp :: !T.Text
    , mh_setSpec :: !T.Text
    } deriving (Show, Eq)

data ArxivSource
    = ArxivSource
    { as_ident :: !T.Text
    , as_texName :: !FilePath
    , as_tex :: !BS.ByteString
    , as_bbl :: !(Maybe BS.ByteString)
    } deriving (Show, Eq)

parseMetaXml :: MonadResource m => Path a File -> Source m MetaHeader
parseMetaXml p =
    parseFile opts (toFilePath p) =$= parseRecords

opts :: ParseSettings
opts =
    def
    { psDecodeEntities = decodeHtmlEntities
    }

parseRecords :: MonadThrow m => Conduit Event m MetaHeader
parseRecords =
    void $ tagIgnoreAttrs "ListRecords" $ manyYield parseRecord

parseRecord :: MonadThrow m => Consumer Event m (Maybe MetaHeader)
parseRecord =
    fmap join $ tagIgnoreAttrs "record" $
    do r <-
           tagNoAttr "header" $
           do mhident <- tagIgnoreAttrs "identifier" content
              mhdatestamp <- tagIgnoreAttrs "datestamp" content
              mhsetSpec <- tagIgnoreAttrs "setSpec" content
              pure $
                  do mh_ident <- mhident
                     mh_datestamp <- mhdatestamp
                     mh_setSpec <- mhsetSpec
                     pure MetaHeader {..}
       void $ ignoreTreeContent "metadata"
       pure $ join r

handleSourceForMeta ::
    (MonadMask m, MonadIO m) => Path a Dir -> MetaHeader -> m (Maybe ArxivSource)
handleSourceForMeta baseDir mh =
    runMaybeT $
    do (strippedIdent, basePath) <- MaybeT (pure $ pathFromIdent $ mh_ident mh)
       logDebug ("Ident: " <> showText strippedIdent <> " Base path: " <> showText basePath)
       let fullPath = baseDir </> basePath
       tarGzHandler strippedIdent fullPath
    where
        extHandler ext fp go =
            do path <- fp <.> ext
               isThere <- liftIO $ doesFileExist path
               if isThere
                   then go path
                   else do logDebug ("File not found: " <> showText path)
                           fail ("File not found: " <> show path)
        gzHandler si tempDir tempLoc =
            do let shellCmd =
                       setWorkingDir (toFilePath tempDir) $
                       shell $ "gunzip " <> toFilePath (filename tempLoc)
               exitCode <- runProcess shellCmd
               case exitCode of
                 ExitFailure code ->
                     do logDebug ("Failed to unpack " <> showText tempLoc <> " with gunzip. Code="
                                 <> showText code
                                 )
                        fail "Can not unpack"
                 ExitSuccess ->
                     do (_, allFiles) <- listDirRecur tempDir
                        case filter (\f -> fileExtension f /= ".gz") allFiles of
                          [] ->
                              do logWarn ("No files found in " <> showText tempDir)
                                 fail "No files found"
                          (x : _) ->
                              do tfContent <- liftIO $ BS.readFile (toFilePath x)
                                 pure
                                     ArxivSource
                                     { as_ident = si
                                     , as_texName = toFilePath (filename x)
                                     , as_tex = tfContent
                                     , as_bbl = Nothing
                                     }
        tarGzHandler si fp =
            extHandler "gz" fp $ \fullPath ->
            lift $ withSystemTempDir "arxivUnpacker" $ \tempDir ->
            do let tempLoc = tempDir </> filename fullPath
               copyFile fullPath tempLoc
               let shellCmd =
                       setWorkingDir (toFilePath tempDir) $
                       shell $ "tar xvzf " <> toFilePath (filename tempLoc)
               exitCode <- runProcess shellCmd
               case exitCode of
                 ExitSuccess ->
                     do (_, allFiles) <- listDirRecur tempDir
                        let texFiles = filter (\f -> fileExtension f == ".tex") allFiles
                            bblFile = find (\f -> fileExtension f == ".bbl") allFiles
                            matchingTexFile =
                                case bblFile of
                                  Just fn -> fn -<.> "tex"
                                  Nothing -> Nothing
                        texFileBbl <- liftIO $ T.mapM doesFileExist matchingTexFile
                        sized <-
                            liftIO $
                            forM texFiles $ \tf ->
                            do sz <- F.fileSize <$> F.getFileStatus (toFilePath tf)
                               hasTitle <-
                                   (\x -> "\\title" `BS.isInfixOf` x)
                                   <$> BS.readFile (toFilePath tf)
                               pure (hasTitle, sz, tf)
                        let texSorted =
                                fmap (\(_, _, x) -> x) $
                                listToMaybe $
                                sortBy (sortPairs flip (\(t, _, _) -> t) flip (\(_, sz, _) -> sz))
                                sized
                        case (if fromMaybe False texFileBbl then matchingTexFile else texSorted) of
                          Nothing ->
                              do logWarn ("No .tex file found in: "
                                          <> showText allFiles <> " of " <> showText fullPath)
                                 fail "No tex file found"
                          Just tf ->
                              do tfContent <- liftIO $ BS.readFile (toFilePath tf)
                                 bblContent <- liftIO $ T.mapM (BS.readFile . toFilePath) bblFile
                                 pure
                                     ArxivSource
                                     { as_ident = si
                                     , as_texName = toFilePath (filename tf)
                                     , as_tex = tfContent
                                     , as_bbl = bblContent
                                     }
                 ExitFailure code ->
                     do logDebug
                            ("Failed to unpack " <> showText tempLoc
                             <> " as .tar.gz (orig: " <> showText fullPath <> "). Code="
                             <> showText code)
                        gzHandler si tempDir tempLoc

sortPairs ::
    (Ord x, Ord a, Monoid b)
    => ((t -> t -> Ordering) -> b)
    -> (t -> a)
    -> ((s -> s -> Ordering) -> b)
    -> (s -> x)
    -> b
sortPairs f x g y = f (comparing x) `mappend` g (comparing y)

pathFromIdent :: T.Text -> Maybe (T.Text, Path Rel File)
pathFromIdent i =
    if idPrefix `T.isPrefixOf` i
    then mkPath (T.drop (T.length idPrefix) i)
    else Nothing
    where
      fullPath :: T.Text -> T.Text -> Maybe (T.Text, Path Rel File)
      fullPath (T.unpack -> prefix) (T.unpack -> counter) =
          do -- myDir <- parseRelDir prefix
             myPrefix <- parseRelFile prefix
             myFile <- myPrefix <.> counter
             pure (T.pack $ prefix ++ "." ++ counter, myFile)
      mkPath :: T.Text -> Maybe (T.Text, Path Rel File)
      mkPath x =
          let (prefix, counter) = second (T.drop 1) $ T.breakOn "." x
          in if T.length prefix == 4 && not (T.null counter)
                then fullPath prefix counter
                else Nothing
      idPrefix =
          "oai:arXiv.org:"
