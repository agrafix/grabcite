{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module GrabCite.DB.Schema where

import Data.Char
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Time
import Hasql.Query
import Hasql.Simple
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Hasql.Decoders as D

data PaperMeta
    = PaperMeta
    { pm_title :: !T.Text
    , pm_dblpUrl :: !(Maybe T.Text)
    , pm_released :: !(Maybe Day)
    } deriving (Show, Eq)

data PaperSearch
    = PaperSearch
    { ps_title :: !T.Text
    , ps_dblpUrl :: !(Maybe T.Text)
    } deriving (Show, Eq)

newtype PaperId
    = PaperId { unPaperId :: Int64 }
    deriving (Show, Eq, DbEncode)

type instance DbRepr PaperId = Int64

instance DbDecode PaperId where
    unpackVal = PaperId

mkQuery :: T.Text -> T.Text
mkQuery =
    T.intercalate " & "
    . map (<> ":*")
    . filter (\x -> T.length x >= 2)
    . filter (T.all isAlpha)
    . T.words

findPaperQ :: Query PaperSearch (V.Vector (PaperId, Double, PaperMeta))
findPaperQ =
    statement sql encoder decoder True
    where
      sql =
          "SELECT id, title, released, dblp_url, "
          <> "(ts_rank_cd(title_vector, query)::float8 + similarity(title, $1)::float8)::float8 AS rank "
          <> "FROM paper, to_tsquery($2) query "
          <> "WHERE title = $1 "
          <> "OR dblp_url = $3 "
          <> "OR query @@ title_vector "
          <> "OR (title % $1 AND similarity(title, $1) > 0.2) "
          <> "ORDER BY rank DESC LIMIT 25"
      encoder =
          req ps_title
          <> req (mkQuery . ps_title)
          <> req (fromMaybe "<DUMMY-DUMMY>" . ps_dblpUrl)
      decoder =
          D.rowsVector $
          do pid <- dbDecVal
             pm_title <- dbDecVal
             pm_dblpUrl <- dbDecOptVal
             pm_released <- dbDecOptVal
             rank <- dbDecVal
             pure (pid, rank, PaperMeta{..})

storePaperMetaQ :: Query PaperMeta PaperId
storePaperMetaQ =
    statement sql encoder decoder True
    where
      sql =
          "INSERT INTO paper (title, released, dblp_url) VALUES ($1, $2, $3) RETURNING id"
      encoder =
          req pm_title
          <> opt pm_released
          <> opt pm_dblpUrl
      decoder =
          D.singleRow dbDecVal

data Author
    = Author
    { a_name :: !T.Text
    } deriving (Show, Eq)

newtype AuthorId
    = AuthorId { unAuthorId :: Int64 }
    deriving (Show, Eq, DbEncode)

type instance DbRepr AuthorId = Int64

instance DbDecode AuthorId where
    unpackVal = AuthorId

findAuthorQ :: Query T.Text (V.Vector (AuthorId, Double, Author))
findAuthorQ =
    statement sql encoder decoder True
    where
      sql =
          "SELECT id, name, "
          <> "(similarity(name, $1)::float8)::float8 AS rank "
          <> "FROM author "
          <> "WHERE name = $1 "
          <> "OR (name % $1 AND similarity(name, $1) > 0.7) "
          <> "ORDER BY rank DESC LIMIT 25"
      encoder =
          req id
      decoder =
          D.rowsVector $
          do aid <- dbDecVal
             a_name <- dbDecVal
             rank <- dbDecVal
             pure (aid, rank, Author{..})

storeAuthorQ :: Query Author AuthorId
storeAuthorQ =
    statement sql encoder decoder True
    where
      sql =
          "INSERT INTO author (name) VALUES ($1) RETURNING id"
      encoder =
          req a_name
      decoder =
          D.singleRow dbDecVal

paperAuthorsQ :: Query PaperId (V.Vector (AuthorId, Author))
paperAuthorsQ =
    statement sql encoder decoder True
    where
      sql =
          "SELECT a.id, a.name "
          <> "FROM author a, paper p, paper_author pa "
          <> "WHERE pa.paper = p.id AND pa.author = a.id AND p.id = $1"
      encoder =
          req id
      decoder =
          D.rowsVector $
          do aid <- dbDecVal
             a_name <- dbDecVal
             pure (aid, Author {..})

linkAuthorQ :: Query (AuthorId, PaperId) ()
linkAuthorQ =
    statement sql encoder decoder True
    where
      sql = "INSERT OR REPLACE INTO paper_author (paper, author) VALUES ($1, $2)"
      encoder = req snd <> req fst
      decoder = D.unit

newtype PaperContentId
    = PaperContentId { unPaperContentId :: Int64 }
    deriving (Show, Eq, DbEncode)

type instance DbRepr PaperContentId = Int64

instance DbDecode PaperContentId where
    unpackVal = PaperContentId

hasContentQ :: Query PaperId (Maybe PaperContentId)
hasContentQ =
    statement sql encoder decoder True
    where
      sql = "SELECT id FROM paper_content where paper = $1"
      encoder = req id
      decoder = D.maybeRow dbDecVal

removeContentQ :: Query PaperContentId ()
removeContentQ =
    statement sql encoder decoder True
    where
      sql = "DELETE FROM paper_content WHERE paper = $1"
      encoder = req id
      decoder = D.unit

data PaperContent
    = PaperContent
    { pc_paper :: !PaperId
    , pc_source :: !T.Text
    , pc_body :: !T.Text
    } deriving (Show, Eq)

writeContentQ :: Query PaperContent PaperContentId
writeContentQ =
    statement sql encoder decoder True
    where
      sql =
          "INSERT INTO paper_content (paper, source, import_date, full_body) "
          <> "VALUES ($1, $2, current_date, $3) "
          <> "RETURNING id"
      encoder =
          req pc_paper
          <> req pc_source
          <> req pc_body
      decoder =
          D.singleRow dbDecVal

newtype PaperSentenceId
    = PaperSentenceId { unPaperSentenceId :: Int64 }
    deriving (Show, Eq, DbEncode)

type instance DbRepr PaperSentenceId = Int64

instance DbDecode PaperSentenceId where
    unpackVal = PaperSentenceId

data PaperSentence
    = PaperSentence
    { ps_sentence :: !T.Text
    , ps_content :: !PaperContentId
    } deriving (Show, Eq)

writeSentenceQ :: Query PaperSentence PaperSentenceId
writeSentenceQ =
    statement sql encoder decoder True
    where
      sql =
          "INSERT INTO paper_sentence (paper_content, sentence) "
          <> "VALUES ($1, $2) "
          <> "RETURNING id"
      encoder =
          req ps_content
          <> req ps_sentence
      decoder =
          D.singleRow dbDecVal

linkSentenceQ :: Query (PaperSentenceId, PaperId) ()
linkSentenceQ =
    statement sql encoder decoder True
    where
      sql =
          "INSERT INTO sentence_reference (sentence, paper) "
          <> "VALUES ($1, $2)"
      encoder =
          req fst <> req snd
      decoder =
          D.unit
