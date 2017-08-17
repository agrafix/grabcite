{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module GrabCite.Annotate
    ( annotateReferences, getPaperId
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
         Just x ->
             case x of
               Left _ ->
                   do logInfo "Cache value was in error state, try to fill it now."
                      go
               Right _ ->
                   do logInfo ("Reading result from cache for " <> showText q)
                      pure x
         Nothing ->
             go
    where
        go =
            do v <- getVal
               atomicModifyIORef' cacheV (\x -> (HM.insert q v x, ()))
               pure v
        cacheV = unRefCache rc

getPaperId :: Show t => RefCache -> Either T.Text [ContentNode t] -> IO (Maybe DblpPaper)
getPaperId refCache ci =
    newManager defaultManagerSettings >>= \mgr ->
    case ci of
      Left pureText ->
          do logInfo ("Finding paper id of " <> showText pureText)
             runLoop (0 :: Int) mgr $
                 (filter (\t -> T.length t > 2) . map T.strip . T.words) pureText
      Right cNodes ->
          do logInfo ("Finding paper id of " <> showText (take 5 cNodes) <> "..")
             case map (isTNode T.strip) $ takeWhile (isTNode $ not . T.null) cNodes of
               [] -> pure Nothing
               textChunks ->
                   runLoop (0 :: Int) mgr $
                   concatMap (filter (\t -> T.length t > 2) . map T.strip . T.words) textChunks
    where
      runLoop !steps mgr wrds =
          if length wrds < 5
          then pure Nothing
          else do let q = snd $ makeSearchQuery False $ T.unwords wrds
                  if T.length q < 5
                     then pure Nothing
                     else do res <- runQuery refCache mgr q
                             case res of
                               Left errMsg ->
                                   do logError (T.pack errMsg)
                                      pure Nothing
                               Right ok ->
                                   case dr_papers ok of
                                     [] ->
                                         if steps > 5
                                         then pure Nothing
                                         else runLoop (steps + 1) mgr (drop 1 wrds)
                                     (paper : _) -> pure (Just paper)
      isTNode f x =
          case x of
            CnText t -> f t
            _ -> f ""

makeSearchQuery :: Bool -> T.Text -> (Bool, T.Text)
makeSearchQuery useYears txt =
    let strWords =
            filter (\t -> T.length t > 2 && T.all isAlpha t) $
            map T.strip $
            T.words (textRemovePunc txt)
        searchQuery = T.unwords strWords
        leftOver = T.drop 40 searchQuery
        years =
            if useYears
            then filter (\t -> T.length t == 4) $ extractYears txt
            else []
        hadYear = not (null years)
    in ( hadYear
       , T.strip $
         T.take 40 searchQuery <> T.takeWhile isAlpha leftOver <> " " <> T.intercalate " " years
       )

runQuery :: RefCache -> Manager -> T.Text -> IO (Either String DblpResult)
runQuery refCache mgr q =
    lookupOrWrite refCache q $
    do r <- try $ queryDblp mgr (DblpQuery q)
       let backupQ e =
               do logInfo $
                      "Query " <> showText q <> " failed to DBLP failed with "
                      <> e <> ", querying PaperGrep"
                  r2 <- try $ queryPaperGrep mgr (DblpQuery q)
                  pure (join $ first showEx r2)
       case join $ first showEx r of
         Right ok@(DblpResult (_ : _)) ->
             do logInfo "Query to DBLP was ok"
                pure (Right ok)
         Right _ -> backupQ "empty result set"
         Left err -> backupQ (showText err)
    where
        showEx :: SomeException -> String
        showEx = show

annotateReferences :: RefCache -> [ContentNode t] -> IO [ContentNode (Maybe DblpPaper)]
annotateReferences refCache contentNodes =
    do mgr <- newManager defaultManagerSettings
       mapM (annotateNode mgr) contentNodes
    where
        annotateNode mgr cn =
            do (hadYear, r) <- annotateNode' True mgr cn
               case r of
                 CnRef (ContentRef { cr_tag = Nothing }) | hadYear ->
                     do logInfo "No tag found, but queried with year. Now trying w/o."
                        snd <$> annotateNode' False mgr cn
                 _ -> pure r
        annotateNode' useYears mgr cn =
            case cn of
              CnText t -> pure (False, CnText t)
              CnRef r ->
                  do let (hadYear, q) = makeSearchQuery useYears (cr_info r)
                     if T.length q < 5
                        then do logWarn $
                                    "Search string to short: " <> showText q
                                pure (hadYear, CnRef $ r { cr_tag = Nothing })
                        else do logInfo ("Will annotate: " <> showText (cr_info r)
                                            <> " Query: " <> showText q)
                                res <- runQuery refCache mgr q
                                case res of
                                  Left errMsg ->
                                      do logError $ T.pack errMsg
                                         pure (hadYear, CnRef $ r { cr_tag = Nothing })
                                  Right ok ->
                                      case dr_papers ok of
                                        (paper : _) ->
                                            do logInfo $
                                                   "Paper is refed by: "
                                                   <> showText (db_url paper)
                                               pure (hadYear, CnRef $ r { cr_tag = Just paper })
                                        _ ->
                                            do logWarn $
                                                   "No search results for " <> showText q
                                               pure (hadYear, CnRef $ r { cr_tag = Nothing })
