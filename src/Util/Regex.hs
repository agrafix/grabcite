module Util.Regex
    ( reM )
where

import Language.Haskell.TH.Quote (QuasiQuoter)
import Text.Regex.PCRE.Heavy
import qualified Text.Regex.PCRE.Light as Re

reM :: QuasiQuoter
reM = mkRegexQQ [Re.multiline]
