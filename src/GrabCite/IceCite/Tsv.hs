{-# LANGUAGE OverloadedStrings #-}
module GrabCite.IceCite.Tsv
    ( parseTsv, parseTsvFile
    , FontWithAttribs(..), IceFeature(..)
    , IceRole(..)
    , IceCiteRow(..)
    )
where

import Data.Bifunctor
import Data.Char
import Data.Csv ((.!))
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as C
import qualified Data.Text as T
import qualified Data.Vector as V

decOpts :: C.DecodeOptions
decOpts =
    C.defaultDecodeOptions
    { C.decDelimiter = fromIntegral (ord '\t')
    }

data FontWithAttribs
    = FontWithAttribs
    { fwa_font :: !T.Text
    , fwa_fontSize :: !Double
    , fwa_color :: !T.Text
    } deriving (Show, Eq)

data IceFeature
    = IfParagraph
    | IfFont
    | IfColor
    deriving (Show, Eq)

instance C.FromField IceFeature where
    parseField s =
        case s of
          "paragraph" -> pure IfParagraph
          "font" -> pure IfFont
          "color" -> pure IfColor
          _ -> fail ("Bad feature: " <> show s)

data IceRole
    = IrReference
    | IrBodyText
    | IrTitle
    | IrFormula
    | IrHeaderOther
    | IrSectionHeading
    | IrAbstractHeading
    | IrAbstract
    | IrPageFooter
    | IrFigure
    | IrFigureCaption
    | IrTable
    | IrTableCaption
    | IrReferenceHeading
    | IrUnknown
    deriving (Show, Eq)

instance C.FromField IceRole where
    parseField s =
        case s of
          "reference" -> pure IrReference
          "reference-heading" -> pure IrReferenceHeading
          "body-text" -> pure IrBodyText
          "title" -> pure IrTitle
          "formula" -> pure IrFormula
          "header-other" -> pure IrHeaderOther
          "section-heading" -> pure IrSectionHeading
          "abstract-heading" -> pure IrAbstractHeading
          "abstract" -> pure IrAbstract
          "page-footer" -> pure IrPageFooter
          "figure" -> pure IrFigure
          "figure-caption" -> pure IrFigureCaption
          "table" -> pure IrTable
          "table-caption" -> pure IrTableCaption
          "unknown" -> pure IrUnknown
          _ -> fail ("Bad role: " <> show s)

data OptIce
    = NoIce
    | IceRow !IceCiteRow
    deriving (Show, Eq)

data IceCiteRow
    = IceCiteRow
    { icr_feature :: !IceFeature
    , icr_text :: !T.Text
    , icr_page :: !Int
    , icr_minX :: !Double
    , icr_minY :: !Double
    , icr_maxX :: !Double
    , icr_maxY :: !Double
    , icr_mostCommon :: !FontWithAttribs
    , icr_start :: !FontWithAttribs
    , icr_end :: !FontWithAttribs
    , icr_role :: !IceRole
    } deriving (Show, Eq)

instance C.FromRecord OptIce where
    parseRecord v
        | length v < 17 = pure NoIce
        | length v == 17 =
              IceRow <$> (IceCiteRow
              <$> v .! 0
              <*> v .! 1
              <*> v .! 2
              <*> v .! 3
              <*> v .! 4
              <*> v .! 5
              <*> v .! 6
              <*> (FontWithAttribs <$> v .! 7 <*> v .! 8 <*> v .! 9)
              <*> (FontWithAttribs <$> v .! 10 <*> v .! 11 <*> v .! 12)
              <*> (FontWithAttribs <$> v .! 13 <*> v .! 14 <*> v .! 15)
              <*> v .! 16)
        | otherwise = fail "Bad ice cite row, not enough cols"

parseTsv :: BS.ByteString -> Either String (V.Vector IceCiteRow)
parseTsv =
    second (V.fromList . mapMaybe go . V.toList) . C.decodeWith decOpts C.HasHeader . BSL.fromStrict
    where
      go x =
          case x of
            NoIce -> Nothing
            IceRow r -> Just r

parseTsvFile :: FilePath -> IO (Either String (V.Vector IceCiteRow))
parseTsvFile fp =
    do bs <- BS.readFile fp
       pure (parseTsv bs)
