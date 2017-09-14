{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module GrabCite.DB
    ( newPgSqlStore, withTempStore
    , Store
    , ExtractionSource(..), handleExtractionResult
    , PaperId(..)
    )
where

import GrabCite.DB.Internal
import GrabCite.DB.Schema
import GrabCite.Dblp
import GrabCite.GetCitations
import GrabCite.GlobalId
import Util.Sentence

import Control.Exception
import Control.Logger.Simple
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.Maybe
import System.Random
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Hasql.Connection as C
import qualified Hasql.Migration as M
import qualified Hasql.Pool as P
import qualified Hasql.Session as S
import qualified Hasql.Transaction as Tx

-- | Temporary postgres store for tests
withTempStore :: (Store -> IO a) -> IO a
withTempStore run =
    bracket allocDb removeDb $ \(_, dbname) ->
    bracket (newPgSqlStore "database" $ "dbname=" <> dbname) (\_ -> pure ()) $ run
    where
        assertRight y =
            case y of
              Right x -> pure x
              Left errMsg -> fail (show errMsg)
        removeDb (globalConn, dbname) =
            do logInfo ("Removing temporary database" <> showText dbname)
               runRes2 <-
                   flip S.run globalConn $ S.sql $ "DROP DATABASE IF EXISTS " <> dbname
               assertRight runRes2
               C.release globalConn
        allocDb =
            do globalConnE <- C.acquire ""
               globalConn <- assertRight globalConnE
               dbnameSuffix <-
                   BSC.pack . take 10 . randomRs ('a', 'z') <$>
                   newStdGen
               let dbname = "chrtest" <> dbnameSuffix
               logInfo ("Creating temporary database" <> showText dbname)
               runRes <-
                   flip S.run globalConn $
                   do S.sql $ "DROP DATABASE IF EXISTS " <> dbname
                      S.sql $ "CREATE DATABASE " <> dbname
               assertRight runRes
               localConnE <- C.acquire $ "dbname=" <> dbname
               localConn <- assertRight localConnE
               runRes' <-
                   flip S.run localConn $
                   do S.sql $ "CREATE EXTENSION hstore"
               assertRight runRes'
               C.release localConn
               pure (globalConn, dbname)

newPgSqlStore :: FilePath -> BS.ByteString -> IO Store
newPgSqlStore migDir connStr =
    do pool <- P.acquire (20, 60 * 5, connStr)
       let store = Store pool
       logInfo $ "Loading and running migrations from " <> showText migDir <>" ..."
       migs <- M.loadMigrationsFromDirectory migDir
       withPool store $
           let loop [] = pure ()
               loop (mig : more) =
                  do liftIO $ logNote ("Will check and or run " <> niceMigration mig)
                     res <-
                         dbTx Tx.ReadCommitted Tx.Write $ M.runMigration mig
                     case res of
                       M.MigrationError err ->
                           fail err
                       M.MigrationSuccess ->
                           do liftIO $ logNote ("Finished " <> niceMigration mig)
                              loop more
           in loop (M.MigrationInitialization : migs)
       logInfo "Migrations complete, store ready"
       pure store

niceMigration :: M.MigrationCommand -> T.Text
niceMigration cmd =
    case cmd of
      M.MigrationInitialization -> "init migration"
      M.MigrationScript s _ -> "script " <> T.pack s
      M.MigrationValidation mc -> "validate (" <> niceMigration mc <> ")"


addLinkAuthorIfNeeded ::
    PaperId
    -> T.Text
    -> Tx.Transaction ()
addLinkAuthorIfNeeded pid authorName =
    do res <-
           Tx.query authorName findAuthorQ
       let nameMatch (_, _, ar) = a_name ar == authorName
           mAid =
               case V.find nameMatch res of
                 Just (aid, _, _) -> Just aid
                 Nothing -> (\(aid, _, _) -> aid) <$> (res V.!? 0)
           link aid =
               Tx.query (aid, pid) linkAuthorQ
       F.mapM_ link mAid

identifyOrInsert ::
    Either (ExtractionResult (Maybe DblpPaper)) (T.Text, Maybe T.Text)
    -> Tx.Transaction (Maybe PaperId)
identifyOrInsert qq =
    do r <- join <$> T.mapM go q
       pid <-
           case r of
             Nothing -> T.mapM goNew q
             Just _ -> pure r
       F.mapM_ authorCheck pid
       pure pid
    where
      authorCheck pid =
          do authors <-
                 fmap (a_name . snd) . V.toList <$> Tx.query pid paperAuthorsQ
             let hasAuthors =
                     case qq of
                       Left ee ->
                           case er_paperId ee of
                             Just dblp -> dp_authors dblp
                             Nothing -> []
                       _ -> []
                 toAdd = hasAuthors L.\\ authors
             mapM_ (addLinkAuthorIfNeeded pid) toAdd
      urlOrTitleMatch ps (_, _, y) =
          case ps_dblpUrl ps of
            Just x -> pm_dblpUrl y == Just x
            Nothing -> pm_title y == ps_title ps
      goNew ps =
          let pm =
                  PaperMeta
                  { pm_title = ps_title ps
                  , pm_dblpUrl = ps_dblpUrl ps
                  , pm_released = Nothing
                  }
          in Tx.query pm storePaperMetaQ
      go ps =
          do res <- Tx.query ps findPaperQ
             case V.find (urlOrTitleMatch ps) res of
               Just (pid, _, _) -> pure (Just pid)
               Nothing -> pure $ (\(pid, _, _) -> pid) <$> (res V.!? 0)
      q =
          case qq of
            Left er ->
                case (er_titleLine er, er_paperId er) of
                  (_, Just dblp) ->
                      Just PaperSearch
                      { ps_title = dp_title dblp
                      , ps_dblpUrl = db_url dblp
                      }
                  (Just tl, _) ->
                      Just PaperSearch
                      { ps_title = tl
                      , ps_dblpUrl = Nothing
                      }
                  _ -> Nothing
            Right (txt, uri) -> Just PaperSearch { ps_title = txt, ps_dblpUrl = uri }

resolveReferences :: [ContentNode GlobalId]
    -> Tx.Transaction [ContentNode (GlobalId, Maybe PaperId)]
resolveReferences cn =
    forM cn $ \n ->
    case n of
      CnText x -> pure (CnText x)
      CnRef cref ->
          do let dblp =
                     case cr_tag cref of
                       GiDblp uri -> Just uri
                       _ -> Nothing
             pid <- identifyOrInsert (Right (cr_info cref, dblp))
             pure $ CnRef $ cref { cr_tag = (cr_tag cref, pid) }

newtype ExtractionSource
    = ExtractionSource { unExtractionSource :: T.Text }
    deriving (Show, Eq)

handleSentence :: PaperContentId -> T.Text -> Tx.Transaction ()
handleSentence pcid sent =
    do psid <- Tx.query (PaperSentence sent pcid) writeSentenceQ
       let pids = getPids sent
       forM_ pids $ \pid -> Tx.query (psid, pid) linkSentenceQ

getPids :: T.Text -> [PaperId]
getPids =
    mapMaybe getPaperId . T.splitOn "<PID:"
    where
      getPaperId textChunk =
          let num = T.takeWhile isDigit textChunk
              more = T.take 1 $ T.dropWhile isDigit textChunk
          in if more == ">" && not (T.null num)
                then Just (PaperId $ read $ T.unpack num)
                else Nothing

writeContent :: ExtractionSource -> PaperId -> (T.Text, [T.Text]) -> Tx.Transaction ()
writeContent esrc pid (fullText, sents) =
    do let pc =
               PaperContent
               { pc_paper = pid
               , pc_source = unExtractionSource esrc
               , pc_body = fullText
               }
       pcid <- Tx.query pc writeContentQ
       mapM_ (handleSentence pcid) sents

handleExtractionResult ::
    Store
    -> Bool
    -> ExtractionSource
    -> ExtractionResult (Maybe DblpPaper)
    -> IO (Maybe PaperId)
handleExtractionResult store shouldOverwriteContent esrc er =
    withPool store $
    dbTx Tx.ReadCommitted Tx.Write $
    do -- step one: identify or insert the paper
       mPid <-
           pureInfo ("Identifying paper " <> showText (er_titleLine er)) $
           identifyOrInsert (Left er)
       case mPid of
         Nothing -> pure ()
         Just pid ->
             do -- step two: handle references
                cnodes <-
                    pureInfo ("Resolving references for " <> showText (er_titleLine er)) $
                    resolveReferences (globalCitId <$> er_nodes er)
                -- handle content
                let fullText = mkTextBody cnodes
                    sents = sentenceSplit fullText
                oldPcid <- Tx.query (pid, unExtractionSource esrc) hasContentQ
                case oldPcid of
                  Just pcid | shouldOverwriteContent ->
                                  do Tx.query pcid removeContentQ
                                     writeContent esrc pid (fullText, sents)
                            | otherwise -> pure ()
                  Nothing -> writeContent esrc pid (fullText, sents)
       pure mPid

mkTextBody :: [ContentNode (GlobalId, Maybe PaperId)] -> T.Text
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
                      let markerText =
                              case cr_tag ref of
                                (gid, Nothing) -> textGlobalId gid
                                (_, Just (PaperId pid)) -> T.pack $ "PID:" ++ show pid
                          refMarker =
                              " <" <> markerText <> "> "
                      in loop more (accum <> TLB.fromText refMarker)
