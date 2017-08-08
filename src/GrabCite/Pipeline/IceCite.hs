-- |
-- Module: GrabCite.Pipeline.IceCite
--
-- Prepare input from an ice cite output
--
{-# LANGUAGE OverloadedStrings #-}
module GrabCite.Pipeline.IceCite
    ( iceCiteJsonAsInput )
where

import GrabCite.IceCite.Types
import GrabCite.Pipeline

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V

iceCiteJsonAsInput :: BS.ByteString -> Either String Input
iceCiteJsonAsInput json =
    do res <- parseIceDocument json
       pure $ InStructured (conv res)

conv :: IceDocument -> StructuredIn
conv doc =
    StructuredIn
    { si_title =
            let t = V.filter (\p -> ip_role p == IrTitle) allParagraphs
            in if V.null t
                  then Nothing
                  else Just (T.intercalate "\n" $ V.toList $ V.map ip_text t)
    , si_textCorpus =
            T.intercalate "\n" $ V.toList $ V.map ip_text $
            V.filter (\p -> ip_role p == IrBodyText) allParagraphs
    , si_referenceCandidates =
            V.map ip_text $
            V.filter (\p -> ip_role p == IrReference) allParagraphs
    }
    where
      allParagraphs = V.concatMap ip_paragraphs $ id_pages doc
