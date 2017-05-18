module GrabCite.Annotate
    ( annotateReferences
    )
where

import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.IORef
import Data.Monoid
import GrabCite.Dblp
import GrabCite.GetCitations
import Network.HTTP.Client
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

textRemovePunc :: T.Text -> T.Text
textRemovePunc = T.map (\ch -> if ch `elem` "()[],.?!-;\'\"" then ' ' else ch)

annotateReferences :: [ContentNode t] -> IO [ContentNode (Maybe DblpPaper)]
annotateReferences contentNodes =
    do mgr <- newManager defaultManagerSettings
       cache <- newIORef HM.empty
       mapM (annotateNode mgr cache) contentNodes
    where
        runQuery mgr cache q =
            do ioHm <- readIORef cache
               case HM.lookup q ioHm of
                 Just r -> pure r
                 Nothing ->
                     do r <- try $ queryDblp mgr (DblpQuery q)
                        writeIORef cache $ HM.insert q r ioHm
                        pure r
        showEx :: SomeException -> String
        showEx = show

        annotateNode mgr cache cn =
            case cn of
              CnText t -> pure (CnText t)
              CnRef r ->
                  do let strWords =
                             filter (\t -> T.length t > 2 && not (T.all isDigit t)) $
                             map T.strip $
                             T.words (textRemovePunc $ cr_info r)
                         searchQuery = T.unwords strWords
                         leftOver = T.drop 30 searchQuery
                         q =
                             T.take 30 searchQuery <> T.takeWhile isAlpha leftOver
                     if T.length q < 5
                        then do putStrLn $
                                    "Search string to short: " ++ show q
                                pure (CnRef $ r { cr_tag = Nothing })
                        else do putStrLn ("Will annotate: " ++ show (cr_info r) ++ " using: "++ show strWords ++ " ... " ++ show q)
                                res <-
                                    runQuery mgr cache q
                                case join $ first showEx res of
                                  Left errMsg ->
                                      do putStrLn errMsg -- poor mans logging
                                         pure (CnRef $ r { cr_tag = Nothing })
                                  Right ok ->
                                      case dr_papers ok of
                                        (paper : _) ->
                                            pure (CnRef $ r { cr_tag = Just paper })
                                        _ -> pure (CnRef $ r { cr_tag = Nothing })
