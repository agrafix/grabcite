module GrabCite.Annotate
    ( annotateReferences
    )
where

import Data.IORef
import GrabCite.Dblp
import GrabCite.GetCitations
import Network.HTTP.Client
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

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
                     do r <- queryDblp mgr (DblpQuery q)
                        writeIORef cache $ HM.insert q r ioHm
                        pure r

        annotateNode mgr cache cn =
            case cn of
              CnText t -> pure (CnText t)
              CnRef r ->
                  do let q = T.take 50 (cr_info r)
                     res <-
                         runQuery mgr cache q
                     case res of
                         Left errMsg ->
                             do putStrLn errMsg -- poor mans logging
                                pure (CnRef $ r { cr_tag = Nothing })
                         Right ok ->
                             case dr_papers ok of
                               (paper : _) ->
                                   pure (CnRef $ r { cr_tag = Just paper })
                               _ -> pure (CnRef $ r { cr_tag = Nothing })
