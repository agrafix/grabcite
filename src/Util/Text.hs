{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Util.Text where

import Data.Char
import qualified Data.Text as T

textRemovePunc :: T.Text -> T.Text
textRemovePunc = T.map (\ch -> if ch `elem` ("()[],.?!-;:\'\"" :: String) then ' ' else ch)

textRemoveLig :: T.Text -> T.Text
textRemoveLig =
    T.replace "\xA761" "vy" .
    T.replace "\xA760" "VY" .
    T.replace "\x1D6B" "ue" .
    T.replace "\xA729" "tz" .
    T.replace "\xA728" "TZ" .
    T.replace "\xA74F" "oo" .
    T.replace "\xA74E" "OO" .
    T.replace "\x0153" "oe" .
    T.replace "\x0152" "OE" .
    T.replace "\x1F670" "et" .
    T.replace "\xA73D" "ay" .
    T.replace "\xA73C" "AY" .
    T.replace "\xA73B" "av" .
    T.replace "\xA73A" "AV" .
    T.replace "\xA739" "av" .
    T.replace "\xA738" "AV" .
    T.replace "\xA737" "au" .
    T.replace "\xA736" "AU" .
    T.replace "\xA735" "ao" .
    T.replace "\xA734" "AO" .
    T.replace "\x00E6" "ae" .
    T.replace "\x00C6" "AE" .
    T.replace "\xA733" "aa" .
    T.replace "\xA732" "AA" .
    T.replace "\64262" "st" .
    T.replace "\64261" "St" .
    T.replace "\64260" "ffl" .
    T.replace "\64258" "fl" .
    T.replace "\64257" "fi" .
    T.replace "\64256" "ff"

getNumberList :: T.Text -> [T.Text]
getNumberList txt =
    let splitted = map T.strip $ T.splitOn "," txt
        cond = all (T.all isDigit) splitted && not (null splitted)
    in if cond then splitted else []


extractYears :: T.Text -> [T.Text]
extractYears txt =
    filter (\grp -> T.all isDigit grp
               && T.length grp  == 4
               && ("20" `T.isPrefixOf` grp || "19" `T.isPrefixOf` grp)) $
    T.groupBy (\d1 d2 -> isDigit d1 && isDigit d2) txt
