{-# Language OverloadedStrings #-}

module Lexer where

import Control.Monad (void)

import Data.Text (Text, singleton)
import Data.Void
import Data.Char

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void (oneOf [' ', '\t'])) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

charLiteral :: Parser Char
charLiteral = lexeme (between (char '\'') (char '\'') L.charLiteral)

stringLiteral :: Parser String
stringLiteral = lexeme (char '"' *> manyTill L.charLiteral (char '"'))

decimal :: Parser Integer
decimal = lexeme L.decimal

binary :: Parser Integer
binary = lexeme L.binary

hexadecimal :: Parser Integer
hexadecimal = lexeme L.hexadecimal

octal :: Parser Integer
octal = lexeme L.octal

float :: Parser Double
float = lexeme L.float

identPred :: Char -> Bool
identPred c = isAlphaNum c || c `elem` ("_'" :: String)

identifier :: Parser Text
identifier = lexeme (mappend <$> (singleton <$> letterChar) <*> takeWhileP Nothing identPred)

typeIdentifier :: Parser Text
typeIdentifier = lexeme (mappend <$> (singleton <$> upperChar) <*> takeWhileP Nothing identPred)

symbol :: Text -> Parser Text
symbol = L.symbol sc

colon :: Parser Char
colon = lexeme (char ':')

comma :: Parser Char
comma = lexeme (char ',')

dot :: Parser Char
dot = lexeme (char '.')

indentBlock = L.indentBlock scn