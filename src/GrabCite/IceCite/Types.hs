{-# LANGUAGE OverloadedStrings #-}
module GrabCite.IceCite.Types
    ( FontWithAttribs(..), IceFeature(..)
    , IceRole(..)
    , IceDocument(..), IcePage(..), IceParagraph(..)
    , parseIceDocument
    )
where

import Data.Aeson
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector as V

parseIceDocument :: BS.ByteString -> Either String IceDocument
parseIceDocument = eitherDecode' . BSL.fromStrict

data IceDocument
    = IceDocument
    { id_pages :: !(V.Vector IcePage)
    } deriving (Show, Eq)

instance FromJSON IceDocument where
    parseJSON =
        withObject "document" $ \o ->
        IceDocument
        <$> o .: "pages"

data IcePage
    = IcePage
    { ip_paragraphs :: !(V.Vector IceParagraph)
    } deriving (Show, Eq)

instance FromJSON IcePage where
    parseJSON =
        withObject "page" $ \o ->
        IcePage
        <$> o .: "paragraph"

data IceParagraph
    = IceParagraph
    { ip_text :: !T.Text
    , ip_page :: !Int
    , ip_minX :: !Double
    , ip_minY :: !Double
    , ip_maxX :: !Double
    , ip_maxY :: !Double
    , ip_mostCommon :: !FontWithAttribs
    , ip_start :: !FontWithAttribs
    , ip_end :: !FontWithAttribs
    , ip_role :: !IceRole
    } deriving (Show, Eq)

instance FromJSON IceParagraph where
    parseJSON =
        withObject "paragraph" $ \o ->
        IceParagraph
        <$> o .: "text"
        <*> o .: "page"
        <*> o .: "minX"
        <*> o .: "minY"
        <*> o .: "maxX"
        <*> o .: "maxY"
        <*> (FontWithAttribs <$> o .: "mostCommonFont"
             <*> o .: "mostCommonFontsize" <*> o .: "mostCommonColor")
        <*> (FontWithAttribs <$> o .: "startFont"
             <*> o .: "startFontsize" <*> o .: "startColor")
        <*> (FontWithAttribs <$> o .: "endFont"
             <*> o .: "endFontsize" <*> o .: "endColor")
        <*> o .: "role"

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

instance FromJSON IceFeature where
    parseJSON =
        withText "feature" $ \s ->
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
    | IrItemizeItem
    | IrUnknown
    deriving (Show, Eq)

instance FromJSON IceRole where
    parseJSON =
        withText "role" $ \s ->
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
          "itemize-item" -> pure IrItemizeItem
          "unknown" -> pure IrUnknown
          _ -> fail ("Bad role: " <> show s)
