{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module GrabCite.Tei
    ( TeiDoc(..)
    , TeiNode(..), _TnText, _TnRef
    , TeiRef(..)
    , TeiHeader(..), TeiBody(..)
    , parseTeiXml
    )
where

import Control.Applicative
import Control.Lens
import Data.Hashable
import Data.List (foldl', find)
import Data.Maybe
import Data.Monoid
import Text.XML as Xml
import Text.XML.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

parseTeiXml :: BS.ByteString -> Either String TeiDoc
parseTeiXml bs =
    case Xml.parseLBS opts (BSL.fromStrict bs) of
      Left errMsg -> Left $ show errMsg
      Right ok -> Right $ parseDoc ok

opts :: ParseSettings
opts =
    def
    { psDecodeEntities = decodeHtmlEntities
    }

data TeiDoc
    = TeiDoc
    { td_header :: !TeiHeader
    , td_body :: !TeiBody
    } deriving (Show, Eq)

newtype TeiRef
    = TeiRef { unTeiRef :: T.Text }
    deriving (Show, Eq, Hashable)

data TeiNode
    = TnText !T.Text
    | TnRef !TeiRef
    deriving (Show, Eq)


allP :: Element -> [TeiNode]
allP anyEl =
    concatMap doNode (elementNodes anyEl)
    where
      doNode node =
          case node ^? _Element of
            Just x | x ^. localName == "p" || x ^. localName == "note" -> handleP x
                   | otherwise -> allP x
            _ -> []

handleP :: Element -> [TeiNode]
handleP eP =
    mapMaybe handleNode (elementNodes eP)
    where
      handleNode node =
          case node of
            NodeContent x -> Just (TnText x)
            NodeElement eRef ->
                case (eRef ^. localName, eRef ^. attribute "type", eRef ^. attribute "target") of
                  ("ref", Just "bibr", Just tgt) -> Just (TnRef $ TeiRef (T.drop 1 tgt))
                  ("ref", Just _, Just _) ->
                      case elementNodes eRef of
                        [] -> Nothing
                        (x : _) -> handleNode x
                  _ -> Nothing
            _ -> Nothing

handleBiblStruct :: HM.HashMap TeiRef T.Text -> Element -> HM.HashMap TeiRef T.Text
handleBiblStruct hm eStruct =
    case find (\(k, _) -> k ^. _nameLocalName == "id") $ M.toList (eStruct ^. attrs) of
      Nothing -> hm
      Just (_, refId) ->
          let key = TeiRef refId
              monogr = eStruct ^? entire . ell "monogr"
              analytic = eStruct ^? entire . ell "analytic"
          in case mkFullText <$> (analytic <|> monogr) of
               Just ft -> HM.insert key ft hm
               Nothing -> hm
    where
      mkFullText x =
          let title = x ^. ell "title" . text
              authors = x ^.. ell "author" ./ ell "persName"
              handleAuthor a =
                  let fname = a ^. ell "forename" . text
                      surname = a ^. ell "surname" . text
                  in fname <> if T.length fname < 3 then "." else "" <> " " <> surname
          in title <> " " <> T.intercalate ", " (map handleAuthor authors)

parseDoc :: Document -> TeiDoc
parseDoc doc =
    let header x = doc ^? root . ell "TEI" ./ ell "teiHeader" ./ x
        th_title =
            header $ ell "fileDesc" ./ ell "titleStmt" ./ ell "title" . text
        th_abstract =
            header $ ell "profileDesc" ./ ell "abstract" ./ ell "p" . text
        body x = doc ^.. root . ell "TEI" ./ ell "text" ./ x
        bodyContent = body $ ell "body"
        backContent =
            body $
            ell "back" ./ ell "div"
            . attributeSatisfies "type" (/= "references")
        tb_content =
           concatMap allP $ bodyContent ++ backContent
        tb_references =
            foldl' handleBiblStruct mempty $ body $
            ell "back" ./ ell "div" . attributeSatisfies "type" (== "references")
            ./ ell "listBibl" ./ ell "biblStruct"
        td_header =
            TeiHeader {..}
        td_body =
            TeiBody {..}
    in TeiDoc {..}


data TeiHeader
    = TeiHeader
    { th_title :: !(Maybe T.Text)
    , th_abstract :: !(Maybe T.Text)
    } deriving (Show, Eq)

data TeiBody
    = TeiBody
    { tb_content :: ![TeiNode]
    , tb_references :: !(HM.HashMap TeiRef T.Text)
    } deriving (Show, Eq)

$(makePrisms ''TeiNode)
