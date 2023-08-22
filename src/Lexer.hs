{-# OPTIONS_GHC -Wno-orphans #-}

module Lexer where

import Control.Monad (void)

import Data.Text (Text, singleton)
import Data.Void (Void)
import Data.Char (isAlphaNum)

import Control.Monad.Reader
import Control.Monad.State

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import Error.Diagnose.Compat.Megaparsec

import Syntax
import NodeId
import AnalysisError

type ParseError = P.ParseErrorBundle Text Void
type Parser = ReaderT [OperatorDef] (P.ParsecT Void Text (State ParserState))

data ParserState = ParserState
    { curNodeId :: NodeId
    , posMap :: PositionMap
    , customParseError :: Maybe AnalysisError -- Reported instead of any parse errors when not Nothing
    } deriving (Show)

instance HasHints Void msg where
  hints _ = mempty
  
reservedNames :: [Text]
reservedNames =
    [
        "fn", "data", "type",
        "let", "in",
        "trait", "impl",
        "if", "then", "else", "match",
        "true", "false",
        "i32", "i64", "f32", "f64", "char", "str", "bool", "unit",
        "import", "export", "module"
    ]

reservedOpers :: [Text]
reservedOpers = ["=", "->", "=>", "\\", "::", ".."]

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space (void P.spaceChar) lineComment P.empty

sc :: Parser ()
sc = L.space (void (P.oneOf [' ', '\t'])) lineComment P.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

endline :: Parser ()
endline = lexeme scn

charLiteral :: Parser Char
charLiteral = lexeme (P.between (P.char '\'') (P.char '\'') L.charLiteral)

stringLiteral :: Parser String
stringLiteral = lexeme (P.char '"' *> P.manyTill L.charLiteral (P.char '"'))

decimal :: Parser Integer
decimal = lexeme L.decimal

binary :: Parser Integer
binary = lexeme (P.string "0" *> L.binary)

hexadecimal :: Parser Integer
hexadecimal = lexeme (P.string "0" *> L.hexadecimal)

octal :: Parser Integer
octal = lexeme (P.string "0" *> L.octal)

float :: Parser Double
float = lexeme L.float

-- signed :: Num a => Parser a -> Parser a
-- signed p = lexeme (L.signed sc p)

identPred :: Char -> Bool
identPred c = isAlphaNum c || c `elem` ("_'" :: String)

operPred :: Char -> Bool
operPred c = c `elem` (":!@#$%^&*-+=<>./?\\|~" :: String)

identifier :: Parser Text
identifier = lexeme (p >>= check)
    where
        p = mappend <$> (singleton <$> P.letterChar) <*> P.takeWhileP (Just "identifier character") identPred
        check x =
            if x `elem` reservedNames
                then fail ("keyword " ++ show x ++ " is reserved")
                else return x

typeIdentifier :: Parser Text
typeIdentifier = lexeme (mappend <$> (singleton <$> P.upperChar) <*> P.takeWhileP Nothing identPred)

operator :: Parser Text
operator = lexeme (p >>= check)
    where
        p = P.takeWhile1P (Just "operator character") operPred
        check x =
            if x `elem` reservedOpers
                then fail ("operator " ++ show x ++ " is reserved")
                else return x

symbol :: Text -> Parser Text
symbol = L.symbol sc

colon :: Parser Char
colon = lexeme (P.char ':')

comma :: Parser Char
comma = lexeme (P.char ',')

dot :: Parser Char
dot = lexeme (P.char '.')

dotNoSpaces :: Parser Char
dotNoSpaces = P.char '.'

parens :: Parser a -> Parser a
parens = P.between (lexeme $ P.char '(') (lexeme $ P.char ')')

braces :: Parser a -> Parser a
braces = P.between (lexeme $ P.char '{') (lexeme $ P.char '}')

indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
indentBlock = L.indentBlock scn
