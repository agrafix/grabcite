{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module GrabCite.GetCitations
    ( extractCitations
    , ExtractionResult(..)
    , CitInfoCand(..), CitMarkerCand(..)
    )
where

import Control.Monad.RWS.Strict
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
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

data ExtractionResult
    = ExtractionResult
    { er_citations :: ![CitInfoCand]
    , er_markers :: ![CitMarkerCand]
    , er_citationFreeText :: !T.Text
    } deriving (Show, Eq)

extractCitations :: T.Text -> ExtractionResult
extractCitations txt =
    let markerCands = collectMarkerCands txt
        withInfo = extractCitInfoLines txt markerCands
        matchedCands = bestCands withInfo
        rMarkers = relevantMarkers matchedCands markerCands
    in ExtractionResult
       { er_citations = matchedCands
       , er_markers = rMarkers
       , er_citationFreeText = removeMarkers txt rMarkers
       }

relevantMarkers :: [CitInfoCand] -> [CitMarkerCand] -> [CitMarkerCand]
relevantMarkers cics =
    filter has
    where
      has cmc =
          flip any (cmc_references cmc) $ \ref -> HS.member (ref, cmc_markerPair cmc) cicSet
      cicSet = HS.fromList $ flip map cics $ \cic -> (cic_ref cic, cmc_markerPair $ cic_marker cic)

removeMarkers :: T.Text -> [CitMarkerCand] -> T.Text
removeMarkers txtIn cmc =
    loop mempty txtIn 0
    where
      loop !accum !txt !pos
          | T.null txt = TL.toStrict $ TLB.toLazyText accum
          | otherwise =
              case HM.lookup pos startPosMap of
                Just elsStartingHere ->
                    let maxIdx = maximum $ map (snd . cmc_range) elsStartingHere
                        len = 1 + (maxIdx - pos)
                    in loop accum (T.drop len txt) (pos + len)
                Nothing -> loop (accum <> TLB.fromText (T.take 1 txt)) (T.drop 1 txt) (pos + 1)
      mkMap hm el =
          HM.insertWith (++) (fst $ cmc_range el) [el] hm
      startPosMap =
          foldl' mkMap HM.empty cmc

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
    , lci_full :: !T.Text
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
extractCitInfoLines txt markerCands =
    catMaybes $
    flip concatMap markerCands $ \mc -> flip map (cmc_references mc) $ \ref ->
    runMarkerCand mc ref
    where
      allLines = fromIntegral $ length (map T.strip $ T.lines txt) + 1
      runMarkerCand mc ref =
          let mp = mkMarkerCandMap mc ref
              (_, ubound) = cmc_range mc
              rawLines = filter (not . T.null) $ map T.strip $ T.lines (T.drop ubound txt)
              skippedLines = allLines - fromIntegral (length rawLines)
              mkPos :: Int -> Double
              mkPos idx =
                  (skippedLines + fromIntegral idx) / allLines
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
              fullScore
                  | lci_full lci `T.isPrefixOf` line = 2
                  | lci_full lci `T.isInfixOf` line = 0.5
                  | otherwise = 0
              lineScore =
                  let wordMatch w pts = if w `T.isInfixOf` line then pts else 0
                  in wordMatch "proceeding" 0.5 + wordMatch "isbn" 0.5
                     + wordMatch "doi" 0.5 + wordMatch "pp." 0.5
              mainScore = (yearScore * 0.5) + (2 * nameScore) + fullScore + lineScore
              totalScore = mainScore + (percPos * 0.3)
          in if mainScore > 1.0 then Just (mkCand line (totalScore, (ref, mc))) else Nothing
      mkMarkerCandMap mc ref =
          let (o, c) = cmc_markerPair mc
              years = extractYears ref
              names =
                  filter (\t -> T.length t >= 2 && t `notElem` years) $
                  map (T.toLower . T.strip . T.filter (/= '.')) $
                  T.split (\ch -> ch == ',' || ch == '&') $ T.replace "et al" "" ref
              full = T.singleton o <> ref <> T.singleton c
          in (LineCitInfo years names full, ref, mc)
