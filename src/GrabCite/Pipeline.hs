module GrabCite.Pipeline where

import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
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

data TextToken
    = TtText !T.Text
    | TtCite !T.Text
    | TtFormula
    deriving (Show, Eq)

data CitedIn
    = CitedIn
    { ci_title :: !T.Text
    , ci_textCorpus :: !(Seq.Seq TextToken)
    , ci_references :: !(HM.HashMap T.Text T.Text)
    } deriving (Show, Eq)

data Input
    = InRawText !RawText
    | InStructured !StructuredIn
    | InCited !CitedIn
    deriving (Show, Eq)
