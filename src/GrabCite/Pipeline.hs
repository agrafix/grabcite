module GrabCite.Pipeline where

import qualified Data.Text as T
import qualified Data.Vector as V

data StructuredIn
    = StructuredIn
    { si_title :: !(Maybe T.Text)
    , si_textCorpus :: !T.Text
    , si_referenceCandidates :: !(V.Vector T.Text)
    } deriving (Show, Eq)

data RawText
    = RawText
    { rt_textCorpus :: !T.Text
    , rt_referenceCorpus :: !(Maybe T.Text)
    } deriving (Show, Eq)

data Input
    = InRawText !RawText
    | InStructured !StructuredIn
    deriving (Show, Eq)
