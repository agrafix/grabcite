{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module GrabCite.GetCitations
    ( extractCitations
    , ExtractionResult(..)
    , CitInfoCand(..), CitMarkerCand(..)
    , ContentNode(..), ContentRef(..)
    , getTextNode, getRefNode
    )
where

import Util.Regex

import Control.Monad.RWS.Strict
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Text.Regex.PCRE.Heavy
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

data CitMarkerCand
    = CitMarkerCand
    { cmc_references :: ![T.Text]
    , cmc_range :: !(Int, Int)
    , cmc_markerPair :: !(Char, Char)
    } deriving (Show, Eq)

data OpenContext
    = OpenContext
    { oc_text :: !TLB.Builder
    , oc_markerPair :: !(Char, Char)
    , oc_start :: !Int
    } deriving (Show, Eq)

type MarkerPairs = HM.HashMap Char Char
data Context
    = Context
    { ctx_open :: !(HM.HashMap Char OpenContext)
    }

markerPairs :: MarkerPairs
markerPairs =
    HM.fromList
    [ ('[', ']')
    , ('(', ')')
    ]

data ContentRef t
    = ContentRef
    { cr_info :: !T.Text
    , cr_origMarker :: !T.Text
    , cr_score :: !Double
    , cr_tag :: !t
    } deriving (Show, Eq)

data ContentNode t
    = CnText !T.Text
    | CnRef !(ContentRef t)
    deriving (Show, Eq)

getTextNode :: ContentNode t -> Maybe T.Text
getTextNode cn =
    case cn of
      CnText x -> Just x
      _ -> Nothing

getRefNode :: ContentNode t -> Maybe (ContentRef t)
getRefNode cn =
    case cn of
      CnRef r -> Just r
      _ -> Nothing

data ExtractionResult t
    = ExtractionResult
    { er_citations :: ![CitInfoCand]
    , er_markers :: ![CitMarkerCand]
    , er_nodes :: ![ContentNode t]
    } deriving (Show, Eq)

extractCitations :: T.Text -> ExtractionResult ()
extractCitations txt =
    let markerCands = collectMarkerCands txt
        withInfo = extractCitInfoLines txt markerCands
        matchedCands = bestCands withInfo
        rMarkers = relevantMarkers matchedCands markerCands
    in ExtractionResult
       { er_citations = matchedCands
       , er_markers = rMarkers
       , er_nodes = toNodes (removeReferencesSection txt) matchedCands rMarkers
       }

removeReferencesSection :: T.Text -> T.Text
removeReferencesSection txtRaw =
    let isRefIntroLine :: T.Text -> Bool
        isRefIntroLine ln
            | not (T.null ln) = ln =~ refSectionIntro
            | otherwise = False
        refSectionLines =
            takeWhile (not . isRefIntroLine . T.strip) $ T.lines txtRaw
    in T.unlines refSectionLines

relevantMarkers :: [CitInfoCand] -> [CitMarkerCand] -> [CitMarkerCand]
relevantMarkers cics =
    filter has
    where
      has cmc =
          flip any (cmc_references cmc) $ \ref -> HS.member (ref, cmc_markerPair cmc) cicSet
      cicSet = HS.fromList $ flip map cics $ \cic -> (cic_ref cic, cmc_markerPair $ cic_marker cic)

toNodes :: T.Text -> [CitInfoCand] -> [CitMarkerCand] -> [ContentNode ()]
toNodes txtIn cics cmcs =
    F.toList $ loop mempty mempty txtIn 0
    where
      mkInfoMap hm cic =
          let marker = cic_marker cic
          in HM.insert (cic_ref cic, cmc_markerPair marker) cic hm
      infoMap =
          foldl' mkInfoMap HM.empty cics
      makeNodes :: [CitMarkerCand] -> [ContentRef ()]
      makeNodes cmc =
          flip concatMap cmc $ \c ->
          flip mapMaybe (cmc_references c) $ \ref ->
          do cic <- HM.lookup (ref, cmc_markerPair c) infoMap
             let (mo, mc) = cmc_markerPair c
             pure
                 ContentRef
                 { cr_info = cic_line cic
                 , cr_origMarker = T.singleton mo <> ref <> T.singleton mc
                 , cr_score = cic_score cic
                 , cr_tag = ()
                 }
      loop !txtAccum !accum !txt !pos
          | T.null txt =
                let collected = TL.toStrict $ TLB.toLazyText txtAccum
                    finalNode =
                        if T.null collected
                        then Seq.empty
                        else Seq.singleton (CnText collected)
                in accum <> finalNode
          | otherwise =
              case HM.lookup pos startPosMap of
                Just elsStartingHere ->
                    let maxIdx = maximum $ map (snd . cmc_range) elsStartingHere
                        len = 1 + (maxIdx - pos)
                        textNode = CnText $ TL.toStrict (TLB.toLazyText txtAccum)
                        refNodes =
                            Seq.fromList $ makeNodes elsStartingHere
                        accum' = accum <> (Seq.singleton textNode <> fmap CnRef refNodes)
                    in loop mempty accum' (T.drop len txt) (pos + len)
                Nothing ->
                    loop (txtAccum <> TLB.fromText (T.take 1 txt)) accum (T.drop 1 txt) (pos + 1)
      mkMap hm el =
          HM.insertWith (++) (fst $ cmc_range el) [el] hm
      startPosMap =
          foldl' mkMap HM.empty cmcs

isCitation :: T.Text -> Bool
isCitation txt =
    let basic =
            T.all isDigit txt
            || "et al." `T.isInfixOf` txt
            || not (null $ extractYears txt)
            || not (null $ getNumberList txt)
        notOnlyAYear =
            case extractYears txt of
              (x:_) -> x /= txt
              _ -> True
    in basic && notOnlyAYear

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

collectMarkerCands :: T.Text -> [CitMarkerCand]
collectMarkerCands txt =
    let (_, citations) =
            execRWS go markerPairs initCtx
        loop (t, pos)
            | T.null t = pure ()
            | otherwise = extractMarkerCands t pos >>= loop
        go =
            loop (txt, 0)
        initCtx =
            Context
            { ctx_open = HM.empty
            }
    in citations

extractMarkerCands ::
    (MonadReader MarkerPairs m, MonadState Context m, MonadWriter [CitMarkerCand] m)
    => T.Text -> Int -> m (T.Text, Int)
extractMarkerCands !txt !pos =
    do mp <- ask
       case T.uncons txt of
         Just (c, more) ->
             do let continue = pure (more, pos + 1)
                modify $ \ctx ->
                    ctx
                    { ctx_open =
                            HM.map (\oc -> oc { oc_text = oc_text oc <> TLB.singleton c }) (ctx_open ctx)
                    }
                case HM.lookup c mp of
                  Just closeChr ->
                      do modify $ \ctx ->
                             ctx
                             { ctx_open =
                                 HM.insert closeChr (OpenContext mempty (c, closeChr) pos) (ctx_open ctx)
                             }
                         continue
                  Nothing ->
                      do ctx <- gets ctx_open
                         case HM.lookup c ctx of
                           Just closedContext ->
                               do tell $
                                      let traw =
                                              T.strip $
                                              TL.toStrict $ TLB.toLazyText $ oc_text closedContext
                                      in if T.null traw
                                         then []
                                         else let v =
                                                      filter isCitation $
                                                      filter (not . T.null) $
                                                      map T.strip . T.splitOn ";" $
                                                      T.take (T.length traw - 1) traw
                                                  v' =
                                                      case v of
                                                        [x] -> case getNumberList x of
                                                                 [] -> v
                                                                 numList -> numList
                                                        _ -> v
                                              in if not (null v)
                                                 then [ CitMarkerCand v'
                                                          (oc_start closedContext, pos)
                                                          (oc_markerPair closedContext)
                                                      ]
                                                 else []
                                  modify $ \ctxr -> ctxr { ctx_open = HM.delete c (ctx_open ctxr) }
                                  continue
                           Nothing -> continue
         Nothing -> pure (T.empty, pos)

data LineCitInfo
    = LineCitInfo
    { lci_years :: ![T.Text]
    , lci_names :: ![T.Text]
    , lci_full :: ![T.Text]
    } deriving (Show, Eq)

matchScore :: [T.Text] -> T.Text -> Double
matchScore search haystack =
    if null search || T.null haystack
    then 0
    else let matches =
                 length $
                 filter (\y -> y `T.isInfixOf` haystack) search
             total = length search
         in (fromIntegral matches / fromIntegral total)

fullScore :: [T.Text] -> T.Text -> Double
fullScore search haystack =
    if null search || T.null haystack
    then 0
    else maximum $ flip map search $ \s ->
         if s `T.isPrefixOf` haystack
         then 2
         else if s `T.isInfixOf` haystack
              then 0.5
              else 0

data CitInfoCand
    = CitInfoCand
    { cic_score :: !Double
    , cic_line :: !T.Text
    , cic_ref :: !T.Text
    , cic_marker :: !CitMarkerCand
    } deriving (Show, Eq)

bestCands :: [CitInfoCand] -> [CitInfoCand]
bestCands =
    mapMaybe bestScore . HM.elems . foldl' grouper HM.empty
    where
      bestScore cic =
          listToMaybe $ sortOn (Down . cic_score) cic
      grouper hm el =
          HM.insertWith (++) (cic_ref el) [el] hm

extractCitInfoLines :: T.Text -> [CitMarkerCand] -> [CitInfoCand]
extractCitInfoLines txtRaw markerCands =
    catMaybes $
    flip concatMap markerCands $ \mc -> flip map (cmc_references mc) $ \ref ->
    runMarkerCand mc ref
    where
      isRefIntroLine :: T.Text -> Bool
      isRefIntroLine ln =
          ln =~ refSectionIntro
      refSectionLines =
          dropWhile (not . isRefIntroLine) $ filter (not . T.null) $
          map T.strip $ T.lines txtRaw
      allLines = fromIntegral $ length (map T.strip $ T.lines txtRaw) + 1
      runMarkerCand mc ref =
          let mp = mkMarkerCandMap mc ref
              (_, ubound) = cmc_range mc
              foundCitSection =
                  not $ null refSectionLines
              rawLines =
                  if foundCitSection
                  then refSectionLines
                  else filter (not . T.null) $ map T.strip $ T.lines (T.drop ubound txtRaw)
              skippedLines = allLines - fromIntegral (length rawLines)
              mkPos :: Int -> Double
              mkPos idx =
                  if foundCitSection
                  then 1
                  else (skippedLines + fromIntegral idx) / allLines
              cicCand =
                  sortOn (Down . cic_score) $ mapMaybe (handleLine mp . second mkPos) (zip rawLines [1..])
          in listToMaybe cicCand
      mkCand line (totalScore, (ref, mc)) =
          CitInfoCand
          { cic_score = totalScore
          , cic_line = line
          , cic_ref = ref
          , cic_marker = mc
          }
      handleLine (lci, ref, mc) (line, percPos) =
          let yearScore = matchScore (lci_years lci) line
              lowerLine = T.toLower line
              nameScore = matchScore (lci_names lci) lowerLine
              fs = fullScore (lci_full lci) line
              lineScore =
                  let wordMatch w pts = if w `T.isInfixOf` line then pts else 0
                  in wordMatch "proceeding" 0.5 + wordMatch "isbn" 0.5
                     + wordMatch "doi" 0.5 + wordMatch "pp." 0.5
              mainScore = (yearScore * 0.5) + (2 * nameScore) + fs + lineScore
              totalScore = mainScore + (percPos * 0.3)
              oneMainMatch =
                  yearScore > 0 || nameScore > 0 || fs > 0.5
          in if mainScore > 1.0 && oneMainMatch
             then Just (mkCand line (totalScore, (ref, mc)))
             else Nothing
      mkMarkerCandMap mc ref =
          let (o, c) = cmc_markerPair mc
              years = extractYears ref
              names =
                  filter (\t -> T.length t >= 2 && t `notElem` years && not (T.all isNumber t)) $
                  map (T.toLower . T.strip . T.filter (/= '.')) $
                  T.split (\ch -> ch == ',' || ch == '&') $ T.replace "et al" "" ref
              fullBase = T.singleton o <> ref <> T.singleton c
              fullMore =
                  if T.all isNumber ref && T.length ref <= 2
                  then [ref <> "."]
                  else []
          in (LineCitInfo years names (fullBase : fullMore), ref, mc)

-- regex taken from parscit
refSectionIntro :: Regex
refSectionIntro =
    [reM|\b(References?|REFERENCES?|Bibliography|BIBLIOGRAPHY|References?\s+and\s+Notes?|References?\s+Cited|REFERENCES?\s+CITED|REFERENCES?\s+AND\s+NOTES?|LITERATURE?\s+CITED?):?\s*$|]
