{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module GrabCite.Annotate
    ( annotateReferences
    , withRefCache, withMemRefCache, RefCache
    )
where

import GrabCite.Dblp
import GrabCite.GetCitations
import GrabCite.PaperGrep
import Util.Text

import Control.Concurrent.Async
import Control.Exception
import Control.Logger.Simple
import Control.Monad
import Data.Aeson
import Data.Bifunctor
import Data.Char
import Data.IORef
import Data.Time.TimeSpan
import Network.HTTP.Client
import Path
import Path.IO
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified System.Directory as Dir

newtype RefCache
    = RefCache
    { unRefCache :: IORef (HM.HashMap T.Text (Either String DblpResult))
    }

sleeper :: IORef Bool -> Int -> IO ()
sleeper stopVar s =
    go s
    where
      go :: Int -> IO ()
      go !secs
          | secs < 0 = pure ()
          | otherwise =
              do stop <- readIORef stopVar
                 logInfo ("Go: " <> showText secs)
                 if stop then pure () else (sleepTS (seconds 1) >> go (secs - 1))

withMemRefCache :: (RefCache -> IO a) -> IO a
withMemRefCache run =
    do c <- newIORef mempty
       run (RefCache c)

withRefCache :: Path x File -> (RefCache -> IO a) -> IO a
withRefCache persistFp action =
    do isThere <- doesFileExist persistFp
       stateVar <-
           if isThere
           then do bsl <- BSL.readFile (toFilePath persistFp)
                   case eitherDecode' bsl of
                     Left errMsg -> fail errMsg
                     Right ok -> newIORef ok
           else newIORef mempty
       stopVar <- newIORef False
       let flush =
               do val <- readIORef stateVar
                  let tmpFile = toFilePath persistFp ++ ".temp"
                  BSL.writeFile tmpFile (encode val)
                  Dir.renamePath tmpFile (toFilePath persistFp)
                  logNote ("Wrote ref cache to " <> showText (toFilePath persistFp))
           start =
               async $
               do let go =
                          do stopNow <- readIORef stopVar
                             flush
                             if stopNow
                                 then pure ()
                                 else (sleeper stopVar 30 >> go)
                  go
           stop x =
               do writeIORef stopVar True
                  logNote "Waiting for ref cache flusher to stop ..."
                  () <- wait x
                  logNote "Everything stopped"
       bracket start stop $ \_ -> action (RefCache stateVar)

lookupOrWrite ::
    RefCache
    -> T.Text
    -> IO (Either String DblpResult)
    -> IO (Either String DblpResult)
lookupOrWrite rc q getVal =
    do val <- readIORef cacheV
       case HM.lookup q val of
         Just x -> pure x
         Nothing ->
             do v <- getVal
                atomicModifyIORef' cacheV (\x -> (HM.insert q v x, ()))
                pure v
    where
        cacheV = unRefCache rc

annotateReferences :: RefCache -> [ContentNode t] -> IO [ContentNode (Maybe DblpPaper)]
annotateReferences refCache contentNodes =
    do mgr <- newManager defaultManagerSettings
       mapM (annotateNode mgr) contentNodes
    where
        runQuery mgr q =
            lookupOrWrite refCache q $
            do r <- try $ queryDblp mgr (DblpQuery q)
               let backupQ =
                       do logInfo "Query to DBLP failed, querying PaperGrep"
                          r2 <- try $ queryPaperGrep mgr (DblpQuery q)
                          pure (join $ first showEx r2)
               case join $ first showEx r of
                 Right ok@(DblpResult (_ : _)) ->
                     do logInfo "Query to DBLP was ok"
                        pure (Right ok)
                 Right _ -> backupQ
                 Left _ -> backupQ

        showEx :: SomeException -> String
        showEx = show

        annotateNode mgr cn =
            case cn of
              CnText t -> pure (CnText t)
              CnRef r ->
                  do let strWords =
                             filter (\t -> T.length t > 2 && T.all isAlpha t) $
                             map T.strip $
                             T.words (textRemovePunc $ cr_info r)
                         searchQuery = T.unwords strWords
                         leftOver = T.drop 40 searchQuery
                         q =
                             T.take 40 searchQuery <> T.takeWhile isAlpha leftOver
                     if T.length q < 5
                        then do logWarn $
                                    "Search string to short: " <> showText q
                                pure (CnRef $ r { cr_tag = Nothing })
                        else do logInfo ("Will annotate: " <> showText (cr_info r)
                                          <> " using: "<> showText strWords
                                            <> " ... " <> showText q)
                                res <-
                                    runQuery mgr q
                                case res of
                                  Left errMsg ->
                                      do logError $ T.pack errMsg
                                         pure (CnRef $ r { cr_tag = Nothing })
                                  Right ok ->
                                      case dr_papers ok of
                                        (paper : _) ->
                                            do logInfo $
                                                   "Paper is refed by: "
                                                   <> showText (db_url paper)
                                               pure (CnRef $ r { cr_tag = Just paper })
                                        _ -> pure (CnRef $ r { cr_tag = Nothing })
