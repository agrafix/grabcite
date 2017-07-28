module Util.Text where

import qualified Data.Text as T

textRemovePunc :: T.Text -> T.Text
textRemovePunc = T.map (\ch -> if ch `elem` "()[],.?!-;:\'\"" then ' ' else ch)
