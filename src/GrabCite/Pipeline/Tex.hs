-- |
-- Module: GrabCite.Pipeline.Tex
--
-- Prepare input from a Tex file
--
{-# LANGUAGE OverloadedStrings #-}
module GrabCite.Pipeline.Tex
    ( texAsInput
      -- * for testing
    , parseTex
      -- * for debugging
    , tryIt
    )
where

import GrabCite.Pipeline

import Control.Monad
import Data.Either
import Data.Void
import Debug.Trace
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
    do i <- T.readFile "test-tex/mu-regular-epsilon.tex"
       case parseTex i True of
         Left errMsg -> putStrLn errMsg
         Right ok -> print ok

parseTex :: T.Text -> Bool -> Either String [Body]
parseTex inp debug =
    case parse ((if debug then dbg "t" else id) docP) "<tex input>" inp of
      Left errMsg -> Left (parseErrorPretty errMsg)
      Right ok -> Right $ simpleBodyList ok

data Body
    = BCmd !Cmd
    | BMath
    | BEnv !Cmd ![Body]
    | BText !T.Text
    | BMany ![Body]
    deriving (Show, Eq)

simplifyBody :: Body -> Body
simplifyBody bdy =
    case bdy of
      BCmd c -> BCmd (simplifyCmd c)
      BMath -> BMath
      BText t -> BText t
      BEnv e b -> BEnv e (simpleBodyList b)
      BMany x -> BMany (simpleBodyList x)

simpleBodyList :: [Body] -> [Body]
simpleBodyList lst =
    case lst of
      (BMany x : more) -> simpleBodyList x ++ simpleBodyList more
      (x : more) -> (simplifyBody x : simpleBodyList more)
      [] -> []

data Cmd
    = Cmd
    { c_name :: !T.Text
    , c_bArgs :: ![[Body]]
    , c_cArgs :: ![[Body]]
    } deriving (Show, Eq)

simplifyCmd :: Cmd -> Cmd
simplifyCmd c =
    c
    { c_bArgs = handleArgs (c_bArgs c)
    , c_cArgs = handleArgs (c_cArgs c)
    }
    where
      handleArgs =
          filter (not . null) . fmap (fmap simplifyBody)

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

commandArgParser :: Parser [Either [Body] [Body]]
commandArgParser =
    many $
    try (Left <$> lexeme (curly (many $ argBodyP True)))
    <|> (Right <$> lexeme (bracket (many $ argBodyP False)))

mkCmd :: T.Text -> [Either [Body] [Body]] -> Cmd
mkCmd name args = Cmd name (lefts args) (rights args)

command :: Parser Cmd
command =
    trace "CMD" $
    do name <- cmdIdent
       args <-
           trace ("args for " ++ show name) $ commandArgParser
       pure $
           trace ("ARGS: " ++ show args) $
           mkCmd name args

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

env :: Parser a -> Parser (Cmd, a)
env action =
    trace "ENV" $
    do name <-
           T.pack <$>
           beginP (some alphaNumChar <* optional (char '*'))
       args <-
           trace ("args for " ++ show name) $ commandArgParser
       r <- trace ("ENV: " ++ show name) action
       endP (symbol name <* optional (char '*'))
       pure (mkCmd name args, r)

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
