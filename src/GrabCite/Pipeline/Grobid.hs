-- |
-- Module: GrabCite.Pipeline.Grobid
--
-- Prepare input from grobid output
--
{-# LANGUAGE OverloadedStrings #-}
module GrabCite.Pipeline.Grobid
    ( grobidXmlAsInput )
where

import GrabCite.Pipeline
import GrabCite.Tei

import Data.Bifunctor
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq

grobidXmlAsInput :: BS.ByteString -> Either String Input
grobidXmlAsInput x =
    do res <- parseTeiXml x
       pure $ InCited (conv res)

conv :: TeiDoc -> CitedIn
conv doc =
    CitedIn
    { ci_title = fromMaybe "" $ th_title $ td_header doc
    , ci_textCorpus = Seq.fromList $ map convTok (tb_content $ td_body doc)
    , ci_references = HM.fromList $ map (first unTeiRef) $ HM.toList (tb_references $ td_body doc)
    }

convTok :: TeiNode -> TextToken
convTok tn =
    case tn of
      TnText x -> TtText x
      TnRef (TeiRef x) -> TtCite x
