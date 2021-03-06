{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module: Util.Tex
--
-- A simple LaTeX parser
--
{-# LANGUAGE OverloadedStrings #-}
module Util.Tex
    ( -- * for testing
      parseTex, parseBbl, Body(..), Cmd(..)
      -- * for debugging
    , tryIt
    )
where

import Control.Monad
import Data.Char
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

tryIt :: IO ()
tryIt =
    do i <- T.readFile "test-tex/mu-regular-epsilon.tex"
       case parseTex i True of
         Left errMsg -> putStrLn errMsg
         Right ok -> print ok

parseTex :: T.Text -> Bool -> Either String [Body]
parseTex inp debug =
    appParser inp debug "<tex input>" docP

parseBbl :: T.Text -> Bool -> Either String [Body]
parseBbl inp debug =
    appParser inp debug "<bbl input>" bibP

appParser ::
    T.Text
    -> Bool
    -> String
    -> Parser [Body]
    -> Either String [Body]
appParser inp debug name p =
    let r = parse ((if debug then dbg "t" else id) p) name inp
    in case r of
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
    , c_cArgs :: ![[Body]]
    , c_bArgs :: ![[Body]]
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
    do x <- some bodyP
       eof
       pure x

bibP :: Parser [Body]
bibP = docExtractP "thebibliography"

docExtractP :: T.Text -> Parser [Body]
docExtractP what =
    do _ <- skipManyTill anyChar (beginP $ symbol what)
       x <- some bodyP
       endP $ symbol what
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
    choice
    [ uncurry BEnv <$> env [] (many bodyP)
    , BMath <$ try math
    , BText <$> try (text True)
    , BCmd <$> try (command False)
    , BMany <$> ([] <$ try comment)
    , BMany <$> curly (many bodyP)
    ]

argBodyP :: Bool -> Parser Body
argBodyP allowBrackets =
    choice
    [ BMath <$ try math
    , BCmd <$> try (command True)
    , BText <$> try (text allowBrackets)
    , BMany <$> ([] <$ try comment)
    , BMany <$> curly (many $ argBodyP True)
    ]

comment :: Parser ()
comment =
    char '%' *> skipManyTill (notChar '\n') (void $ char '\n')

cmdIdent :: Bool -> Parser T.Text
cmdIdent allowBeginEnd = (lexeme . try) (p >>= fixup)
  where
    p :: Parser String
    p =
        do open <- char '\\'
           name <- some (letterChar <|> char '@')
           endCharOpt <- optional (endChar name)
           pure $ (open : name) ++ maybeToList endCharOpt
    brackets =
        char ']'
        <|> char '['
        <|> char ')'
        <|> char '('
    endChar name =
        try $
        char '*' <|>
        case toLower <$> name of
          "big" -> brackets
          "bigg" -> brackets
          _ -> mzero
    fixup x =
        case x of
          "\\begin" | not allowBeginEnd -> fail "Found begin"
          "\\end" | not allowBeginEnd -> fail "Found end"
          _ -> pure (T.drop 1 $ T.pack x)

commandArgParser :: Parser [Either [Body] [Body]]
commandArgParser =
    many $
    try (Left <$> lexeme (curly (many $ argBodyP True)))
    <|> (Right <$> lexeme (bracket (many $ argBodyP False)))

mkCmd :: T.Text -> [Either [Body] [Body]] -> Cmd
mkCmd name args = Cmd (T.toLower name) (lefts args) (rights args)

command :: Bool -> Parser Cmd
command allowBeginEnd =
    do name <- cmdIdent allowBeginEnd
       args <- commandArgParser
       pure $
           mkCmd name args

math :: Parser ()
math =
    doubleDollar
    <|> simpleDollar
    <|> latexShortHand "[" "]"
    <|> latexShortHand "(" ")"
    where
        doubleDollar =
            do _ <- try $ symbol "$$"
               _ <-
                   skipSomeTill
                   (mathVal <|> (char '$' <* notFollowedBy (char '$')))
                   (symbol "$$")
               pure ()
        latexShortHand o c =
            do _ <- try $ symbol ("\\" <> o)
               _ <- skipSomeTill anyChar (try $ symbol ("\\" <> c))
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

mathEnvs :: S.Set T.Text
mathEnvs =
    S.fromList
    [ "math"
    , "displaymath"
    , "equation"
    , "equation*"
    , "eqnarray"
    , "eqnarray*"
    , "align"
    , "align*"
    , "gather"
    , "gather*"
    ]

verbatimEnvs :: S.Set T.Text
verbatimEnvs =
    S.fromList
    [ "verbatim"
    , "highlighting"
    ]

env :: a -> Parser a -> Parser (Cmd, a)
env def action =
    do name <-
           try $ T.pack <$>
           beginP (some alphaNumChar <* optional (char '*'))
       args <- commandArgParser
       let lowerName = T.toLower name
           skipEnvs = verbatimEnvs <> mathEnvs
       (r, parseEnd) <-
           if lowerName `S.member` skipEnvs
           then do _ <-
                       skipManyTill anyChar (try $ endP (symbol name <* optional (char '*')))
                   pure (def, False)
           else do x <- action
                   pure (x, True)
       when parseEnd $ endP (symbol name <* optional (char '*'))
       pure (mkCmd name args, r)

text :: Bool -> Parser T.Text
text allowBrackets =
    T.pack <$> some literalVal
    where
      accents x y =
          try (char '\\' *> char '\'' *> char x *> pure y)
      umlauts x y =
          try (char '\\' *> char '"' *> char x *> pure y)
      escapedChrGen :: Parser Char
      escapedChrGen =
          try $
          do _ <- char '\\'
             x <- satisfy (\c -> not (isAlpha c))
             pure x
      literalVal =
          umlauts 'a' 'ä'
          <|> umlauts 'u' 'ü'
          <|> umlauts 'o' 'ö'
          <|> umlauts 'A' 'Ä'
          <|> umlauts 'U' 'Ü'
          <|> umlauts 'O' 'Ö'
          <|> accents 'e' 'é'
          <|> accents 'E' 'É'
          <|> escapedChrGen
          <|> try (pure ' ' <* char '~')
          <|> satisfy cond
      cond c =
          c /= '\\' && c /= '$'  && c /= '}' && c /= '{' && c /= '%'
          && (if allowBrackets then True else c /= ']' && c /= '[')
