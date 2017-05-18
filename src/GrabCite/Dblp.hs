{-# LANGUAGE OverloadedStrings #-}
module GrabCite.Dblp where

import Data.Aeson
import Data.Monoid
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI
import qualified Blaze.ByteString.Builder as BBB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

data DblpQuery
    = DblpQuery
    { dq_query :: !T.Text
    } deriving (Show, Eq)

data DblpPaper
    = DblpPaper
    { dp_authors :: ![T.Text]
    , dp_title :: !T.Text
    , db_url :: !(Maybe T.Text)
    , db_id :: !T.Text
    , db_score :: !T.Text
    } deriving (Show, Eq)

instance FromJSON DblpPaper where
    parseJSON =
        withObject "DblpPaper" $ \o ->
        do pid <- o .: "@id"
           info <- o .: "info"
           authors <- info .: "authors"
           authorList <- authors .: "author"
           title <- info .: "title"
           url <- info .:? "url"
           score <- o .: "@score"
           pure (DblpPaper authorList title url pid score)

data DblpResult
    = DblpResult
    { dr_papers :: ![DblpPaper]
    } deriving (Show, Eq)

instance FromJSON DblpResult where
    parseJSON =
        withObject "DblpResult" $ \o ->
        do r <- o .: "result"
           hits <- r .: "hits"
           hitList <- hits .: "hit"
           pure (DblpResult hitList)

toQueryUrl :: DblpQuery -> String
toQueryUrl q =
    BSC.unpack $ BBB.toByteString $ "http://dblp.uni-trier.de/search/publ/api" <> qs
    where
      qs =
          renderQueryText True
          [ ( "q", Just (dq_query q) )
          , ( "h", Just "1" )
          , ( "c", Just "0" )
          , ( "format", Just "json" )
          ]


queryDblp :: Manager -> DblpQuery -> IO (Either String DblpResult)
queryDblp manager q =
    do request <-
           parseRequest (toQueryUrl q)
       response <- httpLbs request manager
       let rb = responseBody response
       case statusCode (responseStatus response) of
          200 ->
              case eitherDecode' rb of
                Left errMsg -> pure (Left $ errMsg ++ "\n" ++ show rb)
                Right ok -> pure (Right ok)
          st -> pure (Left $ "Bad status code: " ++ show st)
