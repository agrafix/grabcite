-- |
-- Module: GrabCite.Pipeline.Tex
--
-- Prepare input from a Tex file
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module GrabCite.Pipeline.Tex
    ( texAsInput, TexInput(..)
    )
where

import GrabCite.Pipeline
import Util.Tex

import Control.Logger.Simple
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Traversable as T

data TexInput
    = TexInput
    { ti_doc :: !T.Text
    , ti_bblFile :: !(Maybe T.Text)
    } deriving (Show, Eq)

texAsInput :: TexInput -> Either String Input
texAsInput ip =
    do texAst <- parseTex (ti_doc ip) False
       bibAst <- T.mapM (flip parseBbl False) (ti_bblFile ip)
       let r =
               flip execState initState $
               do mapM_ handleBody texAst
                  case bibAst of
                    Just ast -> handleBib ast
                    Nothing -> pure ()
       pure (InCited r)

type World m = MonadState CitedIn m

initState :: CitedIn
initState =
    CitedIn
    { ci_title = mempty
    , ci_textCorpus = mempty
    , ci_references = mempty
    }

setTitle :: World m => T.Text -> m ()
setTitle t =
    modify' $ \ci ->
    ci { ci_title = t }

pushFormula :: World m => m ()
pushFormula =
    modify' $ \ci ->
    ci { ci_textCorpus = ci_textCorpus ci Seq.|> TtFormula }

pushText :: World m => T.Text -> m ()
pushText t =
    modify' $ \ci ->
    ci { ci_textCorpus = ci_textCorpus ci Seq.|> TtText t }

pushCite :: World m => T.Text -> m ()
pushCite t =
    modify' $ \ci ->
    ci { ci_textCorpus = ci_textCorpus ci Seq.|> TtCite t }

addReference :: World m => T.Text -> T.Text -> m ()
addReference citeId t =
    modify' $ \ci ->
    ci { ci_references = HM.insert citeId t (ci_references ci) }

handleBib :: World m => [Body] -> m ()
handleBib bdy =
    loop bdy bibRef
    where
      bibRef = Nothing
      loop els st =
          case els of
            [] ->
                case st of
                  Just (ref, txt) ->
                      addReference ref txt
                  Nothing -> pure ()
            (x : xs) ->
                case x of
                  BEnv _ _ -> loop xs st
                  BCmd (Cmd "bibitem" cargs _) ->
                      do case st of
                           Just (ref, txt) -> addReference ref txt
                           Nothing -> pure ()
                         case cargs of
                           ((BText v : _) : _) ->
                               loop xs (Just (v, ""))
                           _ ->
                               pureWarn
                               ("Can not understand bibitem: " <> showText x) $
                               loop xs Nothing
                  BCmd _ -> loop xs st
                  BMath -> loop xs st
                  BText t ->
                      case st of
                        Just (ref, txt) ->
                            loop xs $ Just (ref, txt <> t)
                        Nothing -> loop xs st
                  BMany more -> loop (more ++ xs) st

handleBody :: World m => Body -> m ()
handleBody bdy =
    case bdy of
      BEnv (Cmd "itemize" _ _) ib -> mapM_ handleBody ib
      BEnv (Cmd "document" _ _) ib -> mapM_ handleBody ib
      BEnv (Cmd "thebibliography" _ _) ib -> handleBib ib
      BEnv _ _ -> pure ()
      BCmd (Cmd "textbf" cargs _) -> mapM_ handleBody (concat cargs)
      BCmd (Cmd "emph" cargs _) -> mapM_ handleBody (concat cargs)
      BCmd (Cmd "title" cargs _) ->
          case cargs of
            ((BText x : _) : _) -> setTitle $ T.unwords $ T.words x
            _ ->
                pure $ pureWarn ("Bad title: " <> showText bdy) ()
      BCmd (Cmd citeMode cargs _)
          | citeMode == "cite" || citeMode == "citet" || citeMode == "citep" ->
          forM_ cargs $ \arg ->
          case arg of
            (BText x : _) -> mapM_ pushCite (filter (not . T.null) $ T.splitOn "," x)
            _ ->
                pure $ pureWarn ("Bad cite reference: " <> showText bdy) ()
      BCmd _ -> pure ()
      BMath -> pushFormula
      BText t -> pushText t
      BMany more ->
          mapM_ handleBody more
