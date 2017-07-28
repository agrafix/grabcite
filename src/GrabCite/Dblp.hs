{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module GrabCite.Dblp
    ( DblpQuery(..)
    , DblpPaper(..), DblpResult(..)
    , queryDblp
    )
where

import Control.Applicative
import Data.Aeson
import Data.Hashable
import Data.Monoid
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI
import qualified Blaze.ByteString.Builder as BBB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

data DblpQuery
    = DblpQuery
    { dq_query :: !T.Text
    } deriving (Show, Eq, Generic)

instance Hashable DblpQuery

data DblpPaper
    = DblpPaper
    { dp_authors :: ![T.Text]
    , dp_title :: !T.Text
    , db_url :: !(Maybe T.Text)
    , db_score :: !T.Text
    } deriving (Show, Eq, Generic)

instance Hashable DblpPaper

instance FromJSON DblpPaper where
    parseJSON =
        withObject "DblpPaper" $ \o ->
        do info <- o .: "info"
           authors <- info .: "authors"
           authorList <-
               authors .: "author"
               <|> ((:[]) <$> authors .: "author")
           title <- info .: "title"
           url <- info .:? "url"
           score <- o .: "@score"
           pure (DblpPaper authorList title url score)

instance ToJSON DblpPaper where
    toJSON paper =
        object
        [ "@score" .= db_score paper
        , "info" .=
            object
            [ "authors" .=
                object [ "author" .= dp_authors paper ]
            , "title" .= dp_title paper
            , "url" .= db_url paper
            ]
        ]

data DblpResult
    = DblpResult
    { dr_papers :: ![DblpPaper]
    } deriving (Show, Eq, Generic)

instance Hashable DblpResult

instance FromJSON DblpResult where
    parseJSON =
        withObject "DblpResult" $ \o ->
        do r <- o .: "result"
           hits <- r .: "hits"
           hitList <-
               hits .:? "hit" .!= []
           pure (DblpResult hitList)

instance ToJSON DblpResult where
    toJSON papers =
        object
        [ "result" .=
            object
            [ "hits" .=
                object
                [ "hit" .= dr_papers papers
                ]
            ]
        ]

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
    do request <- parseRequest (toQueryUrl q)
       response <- httpLbs request manager
       let rb = responseBody response
       case statusCode (responseStatus response) of
          200 ->
              case eitherDecode' rb of
                Left errMsg -> pure (Left $ errMsg ++ "\n" ++ show rb)
                Right ok -> pure (Right ok)
          st -> pure (Left $ "Bad status code: " ++ show st)
