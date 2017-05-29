module GrabCite.Context
    ( ContextedMarker(..)
    , getContextedMarkers
    )
where

import GrabCite.GetCitations
import GrabCite.GlobalId

import Data.Maybe
import Data.Monoid
import qualified Data.Text as T

data ContextedMarker
    = ContextedMarker
    { mc_before :: !T.Text
    , mc_id :: !GlobalId
    , mc_after :: !T.Text
    } deriving (Show, Eq)

data WorkState
    = WorkState
    { ws_out :: ![ContextedMarker]
    , ws_prev :: ![ContentNode GlobalId]
    , ws_next :: ![ContentNode GlobalId]
    }

getContextedMarkers :: Int -> [ContentNode GlobalId] -> [ContextedMarker]
getContextedMarkers wordsToTake allNodes =
    loop (WorkState [] (reverse allNodes) [])
    where
      loop st =
          case ws_prev st of
            [] -> ws_out st
            (node : rest) ->
                case node of
                  CnText _ ->
                      loop $
                      st
                      { ws_prev = rest
                      , ws_next = (node : ws_next st)
                      }
                  CnRef r ->
                      let st' =
                              st
                              { ws_prev = rest
                              }
                      in let nextText =
                                 takeNodesUntilWords wordsToTake (id, id) (<>) $
                                 mapMaybe getTextNode (ws_next st)
                             prevText =
                                 takeNodesUntilWords wordsToTake (reverse, reverse) (flip (<>)) $
                                 mapMaybe getTextNode (ws_prev st)
                             marker =
                                 ContextedMarker
                                 { mc_before = prevText
                                 , mc_after = nextText
                                 , mc_id = cr_tag r
                                 }
                         in loop $ st'
                            { ws_out = (marker : ws_out st')
                            }

type Bidir a = ([a] -> [a], [a] -> [a])

takeNodesUntilWords :: Int -> Bidir T.Text -> (T.Text -> T.Text -> T.Text) -> [T.Text] -> T.Text
takeNodesUntilWords wordsToTake modList combT textNodes =
    case textNodes of
      [] -> T.empty
      (node : rest) ->
          let (out, missing) = handleTextNode wordsToTake modList node
          in if missing <= 0
             then out
             else out `combT` takeNodesUntilWords missing modList combT rest

handleTextNode :: Int -> Bidir T.Text -> T.Text -> (T.Text, Int)
handleTextNode wordsToTake modList txt =
    let myWords = T.words txt
        missing = wordsToTake - length myWords
    in if missing >= 0
          then (txt, missing)
          else (T.unwords . snd modList . take wordsToTake . fst modList $ myWords, 0)
