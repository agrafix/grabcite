{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Util.CiteSeerX
    ( forAllPapers
    , CsPaperId(..), CsCitId(..)
    , CsCit(..), CsPaper(..)
    )
where

import Control.Error
import Control.Exception
import Control.Logger.Simple
import Control.Monad.Except
import Control.Monad.Reader
import Data.Convertible
import Database.HDBC
import Database.HDBC.ODBC
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

newtype CsPaperId
    = CsPaperId { unCsPaperId :: T.Text }
    deriving (Show, Eq)

newtype CsCitId
    = CsCitId { unCsCitId :: T.Text }
    deriving (Show, Eq)

data CsCit
    = CsCit
    { cc_id :: !CsCitId
    , cc_context :: !T.Text
    , cc_title :: !T.Text
    } deriving (Show, Eq)

data CsPaper
    = CsPaper
    { cp_id :: !CsPaperId
    , cp_title :: !T.Text
    , cp_citations :: !(V.Vector CsCit)
    } deriving (Show, Eq)

forAllPapers :: String -> (CsPaper -> IO ()) -> IO ()
forAllPapers cstr action =
    bracket openConn closeConn $ \conn ->
    do stmt <- prepare conn allQ
       citStmt <- prepare conn citQ
       _ <- execute stmt []
       fetchLoop citStmt stmt
    where
      fetchLoop citStmt stmt =
          loadFetch stmt paperRowParser (pure ()) (pure ()) $ \p ->
          do doPaper citStmt p action
             fetchLoop citStmt stmt

      allQ =
          "SELECT ID, TITLE FROM PAPERS;"
      citQ =
            "SELECT CC.CITATIONID, CC.CONTEXT, C.ARAW FROM CITATIONCONTEXTS CC, CITATIONS C"
            <> "WHERE C.PAPERID = ? AND C.ID = CC.CITATIONID;"
      openConn =
          connectODBC cstr
      closeConn =
          disconnect

loadFetch :: Statement -> RowParser t -> IO x -> IO x -> (t -> IO x) -> IO x
loadFetch stmt parser onNoValue onError onOk =
    do valMap <- fetchRowMap stmt
       case valMap of
         Nothing -> onNoValue
         Just x ->
             case parseResultMap x parser of
               Left errMsg ->
                   do logError ("Failed to parse " <> showText x <> " with " <> errMsg)
                      onError
               Right p -> onOk p

doPaper :: Statement -> (CsPaperId, T.Text) -> (CsPaper -> IO ()) -> IO ()
doPaper citStmt (pid, title) action =
    do _ <- execute citStmt [convert (unCsPaperId pid)]
       cits <- fetchLoop citStmt
       action (CsPaper pid title $ V.fromList cits)
    where
       fetchLoop stmt =
           loadFetch stmt citRowParser (pure []) (pure []) $ \p ->
           do xs <- fetchLoop stmt
              pure (p : xs)


paperRowParser :: RowParser (CsPaperId, T.Text)
paperRowParser =
    (,) <$> (CsPaperId <$> getField "ID") <*> getField "TITLE"

citRowParser :: RowParser CsCit
citRowParser =
    CsCit <$> (CsCitId <$> getField "CITATIONID") <*> getField "CONTEXT" <*> getField "ARAW"

type RowParser a = ExceptT T.Text (Reader (M.Map String SqlValue)) a

getField :: Convertible SqlValue a => String -> RowParser a
getField t =
    lift ask >>= \env ->
    case M.lookup t env of
      Nothing -> throwE ("Missing value for column " <> showText t)
      Just ok ->
        case safeConvert ok of
          Left errMsg -> throwE (showText errMsg)
          Right val -> pure val


parseResultMap :: M.Map String SqlValue -> RowParser a -> Either T.Text a
parseResultMap innerMap parser =
    runReader (runExceptT parser) innerMap
