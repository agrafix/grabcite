{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module GrabCite.GlobalId
    ( globalCitId, textGlobalId
    , GlobalId(..)
    )
where

import GrabCite.Dblp
import GrabCite.GetCitations
import Util.Text

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Monoid
import GHC.Generics
import Text.Regex.PCRE.Heavy
import qualified Data.Text as T

data GlobalId
    = GiDblp !T.Text
    | GiArxiv !T.Text
    | GiDoi !T.Text
    | GiCustom !T.Text
    deriving (Show, Eq, Generic)

instance Hashable GlobalId

textGlobalId :: GlobalId -> T.Text
textGlobalId gi =
    case gi of
      GiDblp x -> "DBLP:" <> x
      GiArxiv x -> "ARXIV:" <> x
      GiDoi x -> "DOI:" <> x
      GiCustom x -> "GC:" <> x

globalCitId :: ContentNode (Maybe DblpPaper) -> ContentNode GlobalId
globalCitId cn =
    case cn of
      CnText txt -> CnText txt
      CnRef ref ->
          let gid =
                  fromMaybe (mkCustom ref) $
                  (GiDblp <$> (join $ db_url <$> cr_tag ref))
                  <|> getDoi ref
                  <|> getArxiv ref
          in CnRef $ ref { cr_tag = gid }

mkCustom :: ContentRef t -> GlobalId
mkCustom cr =
    let hashComps =
            sort $ take 5 $ map T.toLower $
            filter (\t -> T.length t > 2 && T.all isAlpha t) $
            map T.strip $
            T.words (textRemovePunc $ cr_info cr)
    in GiCustom $ T.intercalate "." hashComps

getDoi :: ContentRef t -> Maybe GlobalId
getDoi = fmap GiDoi . reExtract doiRe . cr_info

getArxiv :: ContentRef t -> Maybe GlobalId
getArxiv cr =
    let info = cr_info cr
    in fmap GiArxiv $ reExtract oldArxivRe info <|> reExtract newArxivRe info

reExtract :: Regex -> T.Text -> Maybe T.Text
reExtract regex txt =
    listToMaybe $ fst <$> scan regex txt

oldArxivRe :: Regex
oldArxivRe =
    [re|([a-z]+)(\.[A-Z]+)?\/([0-9]{2})(01|02|03|04|05|06|07|08|09|10|11|12)([0-9]{1,3})|]

newArxivRe :: Regex
newArxivRe =
    [re|([0-9]{2})(01|02|03|04|05|06|07|08|09|10|11|12)\.([0-9]{4,5})(v[0-9]+)?|]

doiRe :: Regex
doiRe =
    [re|\b(10[.][0-9]{4,}(?:[.][0-9]+)*\/(?:(?!["&\'<>])[[:graph:]])+)\b|]
