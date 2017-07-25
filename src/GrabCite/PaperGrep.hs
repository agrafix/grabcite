{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module GrabCite.PaperGrep
    ( queryPaperGrep )
where

import GrabCite.Dblp

import Data.Aeson
import Data.Maybe
import Data.Monoid
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI
import SuperRecord
import qualified Blaze.ByteString.Builder as BBB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Vector as V

type SearchEntry
    = Record
    '[ "key" := T.Text
     , "ty" := T.Text
     , "authors" := V.Vector T.Text
     , "title" := Maybe T.Text
     , "year" := Maybe Int
     , "journal" := Maybe T.Text
     , "url" := Maybe T.Text
     , "ee" := Maybe T.Text
     , "pages" := Maybe T.Text
     , "volume" := Maybe T.Text
     , "editor" := Maybe T.Text
     , "series" := Maybe T.Text
     ]

type SearchResult
    = Record
    '[ "rank" := Double
     , "entry" := SearchEntry
     ]

type SearchResults
    = Record
    '[ "results" := V.Vector SearchResult
     ]

toDblp :: SearchResults -> DblpResult
toDblp sr =
    DblpResult
    { dr_papers = V.toList $ V.map conv $ get #results sr
    }
    where
      conv x =
          let r = get #rank x
              e = get #entry x
          in DblpPaper
             { dp_authors = V.toList (get #authors e)
             , dp_title = fromMaybe "" $ get #title e
             , db_url = Just $ "http://dblp.org/rec/" <> get #key e
             , db_score = T.pack (show r)
             }

toQueryUrl :: DblpQuery -> String
toQueryUrl q =
    BSC.unpack $ BBB.toByteString $ "http://papergrep.com/api/search?" <> qs
    where
      qs =
          renderQueryText True
          [ ( "q", Just (dq_query q) )
          ]


queryPaperGrep :: Manager -> DblpQuery -> IO (Either String DblpResult)
queryPaperGrep manager q =
    do request <- parseRequest (toQueryUrl q)
       response <- httpLbs request manager
       let rb = responseBody response
       case statusCode (responseStatus response) of
          200 ->
              case eitherDecode' rb of
                Left errMsg -> pure (Left $ errMsg ++ "\n" ++ show rb)
                Right ok -> pure (Right $ toDblp ok)
          st -> pure (Left $ "Bad status code: " ++ show st)
