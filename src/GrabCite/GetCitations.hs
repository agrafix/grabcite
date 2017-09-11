{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module GrabCite.GetCitations
    ( extractCitations
    , ExtractionResult(..)
    , CitInfoCand(..), CitMarkerCand(..)
    , ContentNode(..), ContentRef(..)
    , getTextNode, getRefNode
      -- * for testing only
    , isBadRefLine
    , extractCitInfoLines
    , extractRefNames
    , extractYears
    )
where

import GrabCite.Pipeline
import Util.Text
import qualified Data.Set as S

import Control.Logger.Simple
import Control.Monad.RWS.Strict
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Text.EditDistance as TED

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
    } deriving (Show, Eq, Functor)

data ContentNode t
    = CnText !T.Text
    | CnRef !(ContentRef t)
    deriving (Show, Eq, Functor)

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
    { er_paperId :: !t
    , er_titleLine :: !(Maybe T.Text)
    , er_citations :: ![CitInfoCand]
    , er_markers :: ![CitMarkerCand]
    , er_nodes :: ![ContentNode t]
    } deriving (Show, Eq)

extractCitations :: Input -> ExtractionResult ()
extractCitations ip =
    case ip of
      InCited ci -> handlePreCited ci
      InRawText rt -> extractCitations' (Left rt)
      InStructured s -> extractCitations' (Right s)


handlePreCited :: CitedIn -> ExtractionResult ()
handlePreCited ci =
    ExtractionResult
    { er_paperId = ()
    , er_titleLine = Nothing
    , er_citations = mkCitation <$> HM.toList (ci_references ci)
    , er_markers = mkMarker (F.toList $ ci_textCorpus ci)
    , er_nodes = mkNode (F.toList $ ci_textCorpus ci)
    }
    where
      mkRef key =
          case HM.lookup key (ci_references ci) of
            Nothing ->
                pureWarn ("Unknown reference " <> showText key) Nothing
            Just info ->
                Just ContentRef
                { cr_info = info
                , cr_origMarker = "[" <> key <> "]"
                , cr_score = 100
                , cr_tag = ()
                }
      mkNode lst =
          case lst of
            [] -> []
            (tok : more) ->
                let t =
                        case tok of
                          TtFormula -> Just $ CnText "<formula>"
                          TtCite key -> CnRef <$> mkRef key
                          TtText x -> Just $ CnText x
                in case t of
                     Just x -> (x : mkNode more)
                     Nothing -> mkNode more
      mkMarker lst =
          case lst of
            [] -> []
            (tok : more) ->
                case tok of
                  TtCite key ->
                      ( CitMarkerCand [key] (0, 0) ('[', ']')
                      : mkMarker more
                      )
                  _ -> mkMarker more
      mkCitation (key, val) =
          CitInfoCand
          { cic_score = 100
          , cic_line = val
          , cic_ref = key
          , cic_marker =
                  CitMarkerCand
                  { cmc_references = [key]
                  , cmc_range = (0, 0) -- TODO ?
                  , cmc_markerPair = ('[', ']')
                  }
          }

extractCitations' :: Either RawText StructuredIn -> ExtractionResult ()
extractCitations' input =
    let textCorpus =
            case input of
              Right si -> si_textCorpus si
              Left rt -> rt_textCorpus rt
        allMarkerCands =
            collectMarkerCands textCorpus
        countMarkerPair hm mc =
            HM.insertWith (+) (cmc_markerPair mc) 1 hm
        markerPairCount :: HM.HashMap (Char, Char) Int
        markerPairCount =
            foldl' countMarkerPair HM.empty allMarkerCands
        bestPair =
            case sortOn (Down . snd) $ HM.toList markerPairCount of
              (mp, _) : _ -> Just mp
              _ -> Nothing
        markerCands =
            case bestPair of
              Nothing -> allMarkerCands
              Just mp -> filter (\m -> cmc_markerPair m == mp) allMarkerCands
        debugMsg =
            "Text corpus lengths: " <> showText (T.length textCorpus) <> "\n"
            <> "Likely citation bounds: " <> showText bestPair <> "\n"
            <> "Initial marker candidates: " <> showText allMarkerCands <> "\n"
            <> "With info: " <> showText withInfo
        withInfo =
            extractCitInfoLines input markerCands
        matchedCands =
            pureDebug debugMsg $ bestCands withInfo
        rMarkers = relevantMarkers matchedCands markerCands
    in ExtractionResult
       { er_paperId = ()
       , er_titleLine = Nothing
       , er_citations = matchedCands
       , er_markers = rMarkers
       , er_nodes = toNodes textCorpus matchedCands rMarkers
       }

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

extractCitInfoLines ::
    Either RawText StructuredIn -> [CitMarkerCand] -> [CitInfoCand]
extractCitInfoLines input markerCands =
    catMaybes $
    flip concatMap markerCands $ \mc -> flip map (cmc_references mc) $ \ref ->
    runMarkerCand mc ref
    where
      runMarkerCand mc ref =
          let mp = mkMarkerCandMap mc ref
              (_, ubound) = cmc_range mc
              (rawLines, mkPos, preCatRefLine) =
                  case input of
                    Right si ->
                        ( F.toList $ si_referenceCandidates si
                        , const Nothing
                        , True
                        )
                    Left x ->
                        case rt_referenceCorpus x of
                          Just refCorpus ->
                              ( filter (not . T.null) $ map T.strip $ T.lines refCorpus
                              , const Nothing
                              , False
                              )
                          Nothing ->
                              let txtRaw = rt_textCorpus x
                                  rl =
                                      filter (not . T.null) $ map T.strip $ T.lines $
                                      T.drop ubound txtRaw
                                  allLines =
                                      fromIntegral $ length (map T.strip $ T.lines txtRaw) + 1
                                  skippedLines = allLines - fromIntegral (length rawLines)
                              in ( rl
                                 , \(idx :: Int) ->
                                       Just $ (skippedLines + fromIntegral idx) / allLines
                                 , False
                                 )
              isGoodLine (ln, _) =
                  not $ isBadRefLine ln
              cicCand =
                  sortOn (Down . cic_score) $
                  mapMaybe (handleLine preCatRefLine mp . second mkPos) $ filter isGoodLine $
                  zip rawLines [1..]
          in pureDebug
               ("Cands for " <> showText mc <> ": " <> showText cicCand) $
             listToMaybe cicCand
      mkCand line (totalScore, (ref, mc)) =
          CitInfoCand
          { cic_score = totalScore
          , cic_line = line
          , cic_ref = ref
          , cic_marker = mc
          }
      handleLine preCatRefLine (lci, ref, mc) (line, percPos) =
          let yearScore = matchScore (lci_years lci) line
              lowerLine = T.toLower line
              nameScore = matchScore (lci_names lci) lowerLine
              fs = fullScore (lci_full lci) line
              refEdit = prepareEditDist ref
              lineEdit = prepareEditDist line
              refLen = max 1 $ T.length refEdit
              editDistance =
                  TED.restrictedDamerauLevenshteinDistance TED.defaultEditCosts
                  (T.unpack refEdit) (T.unpack $ T.take refLen lineEdit)
              editScore :: Double
              editScore =
                  1 - (fromIntegral editDistance / fromIntegral refLen)
              wordScore =
                  getWordScore refEdit (T.take refLen lineEdit)
              lineScore =
                  let wordMatch w pts = if w `T.isInfixOf` line then pts else 0
                  in wordMatch "proceeding" 0.5 + wordMatch "isbn" 0.5
                     + wordMatch "doi" 0.5 + wordMatch "pp." 0.5
                     + (if preCatRefLine then 0.5 else 0)
              mainScore =
                  (yearScore * 0.5) + (2 * nameScore) + fs + lineScore
                  + editScore * 0.5
                  + wordScore
              totalScore = mainScore + (fromMaybe 0 percPos * 0.3)
              oneMainMatch =
                  yearScore > 0 || nameScore > 0 || fs > 0.5
                  || (editDistance <= 2 && editScore > 0.2)
                  || wordScore > 0.8
          in if mainScore > 1.0 && oneMainMatch
             then Just (mkCand line (totalScore, (ref, mc)))
             else pureDebug ("Bad score for " <> showText ref <> " vs "
                             <> showText line <> ": " <> showText totalScore <> ". Edit dist = "
                             <> showText editDistance <> ". Edit score = " <> showText editScore)
                  Nothing
      mkMarkerCandMap mc ref =
          let (o, c) = cmc_markerPair mc
              years = extractYears ref
              names =
                  extractRefNames years ref
              fullBase = T.singleton o <> ref <> T.singleton c
              fullMore =
                  if T.all isNumber ref && T.length ref <= 2
                  then [ref <> "."]
                  else []
          in (LineCitInfo years names (fullBase : fullMore), ref, mc)

getWordScore :: T.Text -> T.Text -> Double
getWordScore t1 t2 =
    let getWords = filter (\t -> T.length t >= 2) . T.words
        w1 = S.fromList (getWords t1)
        w2 = S.fromList (getWords t2)
        l1 = length (w1 `S.intersection` w2)
        l2 = 1 + (length (w1 `S.union` w2))
    in fromIntegral l1 / fromIntegral l2

prepareEditDist :: T.Text -> T.Text
prepareEditDist x =
    T.unwords $ T.words $ T.map (\c -> if isAlpha c then c else ' ') $ T.toLower x

extractRefNames :: [T.Text] -> T.Text -> [T.Text]
extractRefNames years ref =
    filter (\t -> T.length t >= 2 && t `notElem` years && not (T.all isNumber t)) $
    map (T.toLower . T.strip . T.filter (/= '.')) $
    T.split (\ch -> ch == ',' || ch == '&' || isDigit ch) $ T.replace "et al" "" ref

isBadRefLine :: T.Text -> Bool
isBadRefLine t =
    let goodWord w =
            T.length w >= 2 && T.all isAlpha w
        wrds = T.words t
        num = T.length (T.filter isNumber t)
        tl = T.length t
    in let pnr :: Double
           pnr = fromIntegral num / fromIntegral tl
       in tl < 10 -- less than 10 letters
          || length wrds < 4 -- less than 4 words
          || length (filter goodWord wrds) < 3 -- less than 3 proper words
          || pnr > 0.3 -- more than 30% numbers
