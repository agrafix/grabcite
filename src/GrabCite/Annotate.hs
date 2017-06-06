{-# LANGUAGE BangPatterns #-}
module GrabCite.Annotate
    ( annotateReferences
    , withRefCache, withMemRefCache, RefCache
    )
where

import GrabCite.Dblp
import GrabCite.GetCitations
import Util.Text

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Bifunctor
import Data.Char
import Data.IORef
import Data.Monoid
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
                 putStrLn ("Go: " ++ show secs)
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
                  putStrLn ("Wrote ref cache to " ++ toFilePath persistFp)
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
                  putStrLn "Waiting for ref cache flusher to stop ..."
                  () <- wait x
                  putStrLn "Everything stopped"
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
               pure $ join $ first showEx r

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
                        then do putStrLn $
                                    "Search string to short: " ++ show q
                                pure (CnRef $ r { cr_tag = Nothing })
                        else do putStrLn ("Will annotate: " ++ show (cr_info r)
                                          ++ " using: "++ show strWords ++ " ... " ++ show q)
                                res <-
                                    runQuery mgr q
                                case res of
                                  Left errMsg ->
                                      do putStrLn errMsg -- poor mans logging
                                         pure (CnRef $ r { cr_tag = Nothing })
                                  Right ok ->
                                      case dr_papers ok of
                                        (paper : _) ->
                                            pure (CnRef $ r { cr_tag = Just paper })
                                        _ -> pure (CnRef $ r { cr_tag = Nothing })
