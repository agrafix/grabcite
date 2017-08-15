-- |
-- Module: GrabCite.Pipeline.Tex
--
-- Prepare input from a Tex file
--
{-# LANGUAGE OverloadedStrings #-}
module GrabCite.Pipeline.Tex
    ( texAsInput, tryIt )
where

import Control.Monad
import Data.Maybe
import Debug.Trace
import GrabCite.Pipeline

import Data.Either
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

texAsInput :: T.Text -> Input
texAsInput = undefined

tryIt :: IO ()
tryIt =
    do i <- T.readFile "test-tex/1010.2903.tex"
       parseTest (dbg "t" docP) i

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "%"
    blockCmnt =
        L.skipBlockComment "\\begin{comment}" "\\end{comment}"
        <|> L.skipBlockComment "\\begin {comment}" "\\end {comment}"
        <|> L.skipBlockComment "\\iffalse" "\\fi"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

curly :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")

bracket :: Parser a -> Parser a
bracket = between (symbol "[") (symbol "]")

docP :: Parser [Body]
docP =
    do _ <- skipManyTill anyChar (beginP $ symbol "document")
       x <- some bodyP
       endP $ symbol "document"
       pure x

beginP :: Parser a -> Parser a
beginP s =
    do _ <- try $ symbol "\\begin"
       curly s

endP :: Parser a -> Parser ()
endP s =
    do _ <- symbol "\\end"
       _ <- curly s
       pure ()

data Body
    = BCmd !Cmd
    | BMath
    | BEnv !T.Text ![Body]
    | BText !T.Text
    | BMany ![Body]
    deriving (Show, Eq)

bodyP :: Parser Body
bodyP =
    trace "BODY" $
    choice
    [ uncurry BEnv <$> env (many bodyP)
    , BMath <$ try math
    , BText <$> try (text True)
    , BCmd <$> try command
    , BMany <$> ([] <$ try comment)
    , BMany <$> curly (some bodyP)
    ]

argBodyP :: Bool -> Parser Body
argBodyP allowBrackets =
    trace "BODY ARG" $
    choice
    [ BCmd <$> try command
    , BMath <$ try math
    , BText <$> try (text allowBrackets)
    , BMany <$> ([] <$ try comment)
    , BMany <$> curly (some $ argBodyP True)
    ]

data Cmd
    = Cmd
    { c_name :: !T.Text
    , c_bArgs :: ![[Body]]
    , c_cArgs :: ![[Body]]
    } deriving (Show, Eq)

comment :: Parser ()
comment =
    char '%' *> skipManyTill (notChar '\n') (void $ char '\n')

cmdIdent :: Parser T.Text
cmdIdent = (lexeme . try) (p >>= fixup)
  where
    p :: Parser String
    p = (:) <$> char '\\' <*> some letterChar
    fixup x =
        do case x of
             "\\begin" -> fail "Found begin"
             "\\end" -> fail "Found end"
             _ -> pure (T.drop 1 $ T.pack x)

command :: Parser Cmd
command =
    trace "CMD" $
    do name <- cmdIdent
       args <-
           trace ("args for " ++ show name) $
           let withArgs =
                   many $
                   try (Left <$> lexeme (curly (many $ argBodyP True)))
                   <|> (Right <$> lexeme (bracket (many $ argBodyP False)))
           in withArgs
       pure $
           trace ("ARGS: " ++ show args) $
           Cmd name (lefts args) (rights args)

math :: Parser ()
math =
    trace "MATH" $
    doubleDollar <|> simpleDollar
    where
        doubleDollar =
            do _ <- try $ symbol "$$"
               _ <-
                   skipSomeTill
                   (mathVal <|> (char '$' <* notFollowedBy (char '$')))
                   (symbol "$$")
               pure ()
        simpleDollar =
            do _ <- try $ char '$'
               _ <- skipSomeTill mathVal (char '$')
               pure ()
        mathVal =
            try (char '\\' *> char '$')
            <|> satisfy cond
        cond c =
            c /= '$'

env :: Parser a -> Parser (T.Text, a)
env action =
    trace "ENV" $
    do name <-
           T.pack <$>
           beginP (some alphaNumChar <* optional (char '*'))
       r <- trace ("ENV: " ++ show name) action
       endP (symbol name <* optional (char '*'))
       pure (name, r)

text :: Bool -> Parser T.Text
text allowBrackets =
    trace "TEXT" $
    T.pack <$> some literalVal
    where
      literalVal =
          try (char '\\' *> char '\\')
          <|> try (char '\\' *> char '$')
          <|> try (char '\\' *> char '{')
          <|> try (char '\\' *> char '}')
          <|> try (char '\\' *> char ' ')
          <|> try (char '\\' *> char '\t')
          <|> try (char '\\' *> char '"')
          <|> try (char '\\' *> char '%')
          <|> satisfy cond
      cond c =
          c /= '\\' && c /= '$'  && c /= '}' && c /= '{' && c /= '%'
          && (if allowBrackets then True else c /= ']' && c /= '[')
