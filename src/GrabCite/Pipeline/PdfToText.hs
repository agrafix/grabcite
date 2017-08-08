-- |
-- Module: GrabCite.Pipeline.PdfToText
--
-- Prepare input from a pdftotext output
--
{-# LANGUAGE QuasiQuotes #-}
module GrabCite.Pipeline.PdfToText
    ( pdfToTextAsInput )
where

import GrabCite.Pipeline
import Util.Regex
import Util.Text

import Text.Regex.PCRE.Heavy
import qualified Data.Text as T

pdfToTextAsInput :: T.Text -> Input
pdfToTextAsInput raw =
    let preparedText = textRemoveLig raw
    in InRawText $ uncurry RawText (splitByRefSection preparedText)

splitByRefSection :: T.Text -> (T.Text, Maybe T.Text)
splitByRefSection txtRaw =
    let isRefIntroLine :: T.Text -> Bool
        isRefIntroLine ln
            | not (T.null ln) = ln =~ refSectionIntro
            | otherwise = False
        textCorpus =
            takeWhile (not . isRefIntroLine . T.strip) $ T.lines txtRaw
        refSection =
            filter (not . T.null) $
            dropWhile (not . isRefIntroLine . T.strip) $ T.lines txtRaw
    in (T.unlines textCorpus, if null refSection then Nothing else Just (T.unlines refSection))

-- regex taken from parscit
refSectionIntro :: Regex
refSectionIntro =
    [reM|\b(References?|REFERENCES?|Bibliography|BIBLIOGRAPHY|References?\s+and\s+Notes?|References?\s+Cited|REFERENCES?\s+CITED|REFERENCES?\s+AND\s+NOTES?|LITERATURE?\s+CITED?):?\s*$|]
