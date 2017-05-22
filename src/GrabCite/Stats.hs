module GrabCite.Stats
    ( getCitStats
    , CitationStats(..)
    )
where

import GrabCite.GetCitations

import Data.List
import qualified Data.Text as T

data CitationStats
    = CitationStats
    { cs_words :: !Int
    , cs_anchors :: !Int
    } deriving (Show, Eq)

getCitStats :: ExtractionResult r -> CitationStats
getCitStats er =
    foldl' mkStats (CitationStats 0 0) (er_nodes er)
    where
      mkStats cs node =
          case node of
            CnRef _ -> cs { cs_anchors = (cs_anchors cs + 1) }
            CnText txt -> cs { cs_words = (cs_words cs + (length $ T.words txt )) }
