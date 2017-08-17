{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Util.Sentence
    ( sentenceSplit
    )
where

import Util.Regex

import Control.Logger.Simple
import Data.Char
import Data.Maybe
import Text.Regex.PCRE.Heavy
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

-- | Split a text blob into sentences. This is highly optimized for grabcite output
-- and may not perform well on general text.
sentenceSplit :: T.Text -> [T.Text]
sentenceSplit txtIn =
    mapMaybe cleanSentence $
    filter isValidSentence $
    let origSplits = sSplit sentenceDecide txtIn
    in pureDebug ("Original splits: " <> showText origSplits) origSplits

sSplit :: (Char -> (T.Text, T.Text) -> Bool) -> T.Text -> [T.Text]
sSplit decisionFun txtIn =
    pureDebug ("Text blob: " <> showText txtIn) $
    filter (not . T.null) $ map (TL.toStrict . TL.strip) $ reverse $ go [] mempty txtIn
    where
      go output current state =
          case T.uncons state of
            Nothing -> (TLB.toLazyText current : output)
            Just (c, more)
                | c == '.' || c == '?' || c == '!' ->
                      let curr = TLB.toLazyText current
                          currL = TL.length curr
                          toDrop = currL - 20
                          alreadyRead = TL.toStrict $ TL.drop (min 0 toDrop) curr
                          afterDot = T.take 20 more
                          shouldSplit = decisionFun c (alreadyRead, afterDot)
                      in if shouldSplit
                            then go (curr <> TL.singleton c : output) mempty more
                            else go output (current <> TLB.singleton c) more
                | otherwise -> go output (current <> TLB.singleton c) more

cleanSentence :: T.Text -> Maybe T.Text
cleanSentence tx =
    let tLines = filter goodLine . T.lines $ tx
        lineCount = length tLines
    in if lineCount == 0 || lineCount > 10
       then pureDebug ("Bad sentence: " <> showText tx) Nothing
       else let s = gsub multiSpace (T.singleton ' ') $ T.intercalate " " tLines
            in if badSentence s then Nothing else (Just s)
    where
      badSentence t =
          let wrds = T.words t
              punct = T.length (T.filter (\c -> isPunctuation c && c /= '/' && c /= '<') t)
              num = T.length (T.filter isNumber t)
              tl = T.length t
              pnr :: Double
              pnr =
                  (fromIntegral punct + fromIntegral num) / fromIntegral tl
              pwr :: Double
              pwr = fromIntegral punct / fromIntegral (length wrds)
          in tl < 10 -- less than 10 letters
               || length wrds < 3 -- less than 3 words
               || length (filter goodWord wrds) < 2 -- less than 2 proper words
               || pnr > 0.3 -- more than 30% numbers / punctuation
               || pwr > 0.6 -- more than 0.6 punctuation characters per word
      goodWord w =
          T.length w >= 2 && T.all isAlpha w
      goodLine x =
          not (T.null x) && not (x =~ sectionTitleLine)

isValidSentence :: T.Text -> Bool
isValidSentence txt
    | T.any (\c -> c =='\f' || c == '^' || c == '@') txt = False
    | otherwise = True

sentenceDecide :: Char -> (T.Text, T.Text) -> Bool
sentenceDecide c (before, after)
    | c == '!' = True
    | c == '?' = True
    | localDots beforeRev || localDots after = False
    | noSpace beforeRev && noSpace after = False
    | noSpace beforeRev && beginsCapital after = True
    | isAbbrev beforeRev = False
    | otherwise = True
    where
      beginsCapital =
          T.all isUpper . T.take 1 . T.strip
      noSpace =
          T.all (not . isSpace) . T.take 1
      isAbbrev x =
          let l = T.takeWhile (not . isSpace) $ T.strip x
          in T.length l <= 3 || (T.all isUpper l && T.length l < 5)
      localDots =
          T.any (\cx -> cx =='.' || cx == ',') . T.take 3
      beforeRev = T.reverse before

sectionTitleLine :: Regex
sectionTitleLine =
    [reM|^([0-9]+(\.[0-9]+(\.[0-9]+(\.[0-9]+)?)?)?\s+)?[A-Z &\-,\.]+$|]

multiSpace :: Regex
multiSpace =
    [re|\s\s+|]
