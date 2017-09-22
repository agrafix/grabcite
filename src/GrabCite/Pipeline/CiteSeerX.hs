{-# LANGUAGE OverloadedStrings #-}
module GrabCite.Pipeline.CiteSeerX where

import GrabCite.Pipeline

import Data.Monoid
import qualified Data.Text as T

newtype CsCitId
    = CsCitId { unCsCitId :: T.Text }
    deriving (Show, Eq)

data CsContext
    = CsContext
    { cc_pre :: !T.Text
    , cc_cit :: !T.Text
    , cc_post :: !T.Text
    } deriving (Show, Eq)

parseCsContext :: T.Text -> Maybe CsContext
parseCsContext i =
    let (pre, citIm) = T.breakOn "=-=" i
        (cit, post) = T.breakOn "-=-" (T.drop 3 citIm)
    in if T.null cit
          then Nothing
          else Just (CsContext pre cit $ T.drop 3 post)

takeDrop :: Int -> T.Text -> (T.Text, T.Text)
takeDrop cond x =
    ( T.take cond x
    , T.drop cond x
    )

findWordMerge :: T.Text -> T.Text -> Maybe T.Text
findWordMerge a b =
    go "" a b
    where
      go lo x y =
          if x `T.isPrefixOf` y
          then Just (lo <> y)
          else let (dropped, more) = takeDrop 1 x
               in if T.null (T.strip more)
                     then Nothing
                     else go (lo <> dropped) more y
mergeContexts :: [(CsCitId, CsContext)] -> [TextToken]
mergeContexts z =
    go Nothing z
    where
      go lst css =
          case css of
            [] ->
                case lst of
                  Nothing -> []
                  Just lastCtx -> [TtText (cc_post lastCtx)]
            (cit, ctx) : more ->
                case lst of
                  Nothing ->
                      ( TtText (cc_pre ctx)
                       : (TtCite . unCsCitId) cit
                       : go (Just ctx) more
                      )
                  Just lastCtx
                      | lastCtx == ctx -> ((TtCite . unCsCitId) cit : go (Just ctx) more)
                      | otherwise ->
                            getTextToks lastCtx ctx
                            ++ [(TtCite . unCsCitId) cit]
                            ++ go (Just ctx) more

getTextToks :: CsContext -> CsContext -> [TextToken]
getTextToks lastCtx ctx =
    let mPreMerge =
            findWordMerge (cc_pre lastCtx <> cc_cit lastCtx) (cc_pre ctx)
        mCitPostMerge =
            findWordMerge (cc_cit lastCtx <> cc_post lastCtx) (cc_pre ctx)
        mPostMerge =
            findWordMerge (cc_post lastCtx) (cc_pre ctx)
        textToks =
            case (mPreMerge, mCitPostMerge, mPostMerge) of
              (Just preM, _, _) ->
                  let toAdd =
                          T.drop (T.length $ cc_pre lastCtx <> cc_cit lastCtx) preM
                  in [TtText toAdd]
              (_ , Just citPost, _) ->
                  let toAdd =
                          T.drop (T.length $ cc_cit lastCtx) citPost
                  in [TtText toAdd]
              (_ , _, Just postM) ->
                  [TtText postM]
              (Nothing, Nothing, Nothing) ->
                  [ TtText (cc_post lastCtx)
                  , TtText (cc_pre ctx)
                  ]
    in textToks
