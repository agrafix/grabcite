{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module: GrabCite.Pipeline.PdfToText
--
-- Prepare input from a pdftotext output
--
module GrabCite.Pipeline.Tex
    ( texAsInput
    , testIt
    )
where

import GrabCite.Pipeline
import Util.Text

import Control.Monad.State hiding (State)
import Control.Monad.Trans.RWS hiding (gets, tell)
import Control.Monad.Writer
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Builder as TLB

texAsInput :: T.Text -> Input
texAsInput = undefined

parseTex :: T.Text -> Either ParseError LaTeX
parseTex inp = parseLaTeX inp

data Output
    = Output
    { o_textCorpus :: !TLB.Builder
    , o_abstract :: !TLB.Builder
    } deriving (Show, Eq)

instance Monoid Output where
    mempty = Output mempty mempty
    mappend a b =
        Output
        { o_textCorpus =
                o_textCorpus a <> o_textCorpus b
        , o_abstract =
                o_abstract a <> o_abstract b
        }

data Env
    = EDocument
    | EAbstract
    | EItemize

data State
    = State
    { s_stack :: !(Seq.Seq Env)
    }

initState :: State
initState = State mempty

type World m = ( MonadState State m, MonadWriter Output m )

pushEnv :: World m => Env -> m ()
pushEnv e =
    modify' $ \s -> s { s_stack = e Seq.<| s_stack s }

popEnv :: World m => Env -> m ()
popEnv e =
    modify' $ \s ->
    case Seq.viewl (s_stack s) of
      Seq.EmptyL -> s
      _ Seq.:< xs -> s { s_stack = xs }

withEnv :: World m => Env -> m r -> m r
withEnv e a =
    do pushEnv e
       x <- a
       popEnv e
       pure x

getEnv :: World m => m (Maybe Env)
getEnv =
    gets $ \s ->
    case Seq.viewl (s_stack s) of
      Seq.EmptyL -> Nothing
      x Seq.:< _ -> Just x

runExtraction :: LaTeX -> Output
runExtraction x =
    snd $ execRWS (traversalExtract x) () initState

traversalExtract ::
    ( World m )
    => LaTeX -> m ()
traversalExtract lt =
    case lt of
      TeXRaw raw ->
          handleRaw raw
      TeXEnv envName args body ->
          handleEnv envName args body
      TeXComm _ _ -> pure () -- todo
      TeXCommS _ -> pure () -- todo
      TeXMath _ _ -> pure () -- todo
      TeXLineBreak _ _ -> pure ()
      TeXBraces x -> traversalExtract x
      TeXComment _ -> pure ()
      TeXSeq a b ->
          do traversalExtract a
             traversalExtract b
      TeXEmpty -> pure ()

handleRaw :: World m => T.Text -> m ()
handleRaw t =
    do env <- getEnv
       case env of
         Just EAbstract -> tell (mempty { o_abstract = TLB.fromText t })
         Just EDocument -> tell (mempty { o_textCorpus = TLB.fromText t })
         Just EItemize -> tell (mempty { o_textCorpus = TLB.fromText t })
         Nothing -> pure ()

handleEnv :: World m => String -> [TeXArg] -> LaTeX -> m ()
handleEnv envName _ body =
    case envName of
      "abstract" -> withEnv EAbstract (traversalExtract body)
      "document" -> withEnv EDocument (traversalExtract body)
      "itemize" -> withEnv EItemize (traversalExtract body)
      _ -> pure ()

testIt :: IO ()
testIt =
    do f <- T.readFile "1010.2903.tex"
       case parseTex f of
         Left err -> fail (show err)
         Right ok ->
             do print ok
                putStrLn "================================="
                print (runExtraction ok)
