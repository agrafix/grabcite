-- |
-- Module: GrabCite.Pipeline.IceCiteBasic
--
-- Prepare input from an ice cite output. Ignore most given roles and only use
-- the reference section heading for breaking up the input
--
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module GrabCite.Pipeline.IceCiteBasic
    ( iceCiteJsonAsBasicInput )
where

import GrabCite.IceCite.Types
import GrabCite.Pipeline

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V

iceCiteJsonAsBasicInput :: BS.ByteString -> Either String Input
iceCiteJsonAsBasicInput json =
    do res <- parseIceDocument json
       pure $ InRawText (conv res)

conv :: IceDocument -> RawText
conv doc =
    RawText
    { rt_textCorpus =
            toText text
    , rt_referenceCorpus =
            if V.null ref then Nothing else (Just (toText ref))
    }
    where
      toText = T.intercalate "\n" . V.toList . V.map ip_text
      (text, ref) =
          V.break (\p -> ip_role p == IrReferenceHeading) allParagraphs
      allParagraphs =
          V.filter removeNoise $
          V.concatMap ip_paragraphs $ id_pages doc
      removeNoise (ip_role -> r) =
          r /= IrFormula
          && r /= IrFigure
          && r /= IrFigureCaption
          && r /= IrTable
          && r /= IrTableCaption
          && r /= IrReferenceHeading
