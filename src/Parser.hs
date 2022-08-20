{-# Language OverloadedStrings, TypeApplications, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where
 
import Data.Text (Text)
import Data.Functor.Identity

import Text.Megaparsec hiding (ParseError)
import qualified Text.Megaparsec.Char.Lexer as L

import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec

import Lexer
import Syntax
import Type

parseTopDeclaration :: Parser UntypedTopDeclaration
parseTopDeclaration = TDDeclaration <$> parseDeclaration

parseDeclaration :: Parser UntypedDeclaration
parseDeclaration = parseFunctionDeclaration

-- Desugars to a let + lambda
parseFunctionDeclaration :: Parser UntypedDeclaration
parseFunctionDeclaration = do
    symbol "fn"
    functionName <- identifier
    typeAnnotation <- optional parseTypeAnnotation
    definition <- parseFunctionDefinition

    let letDefinition = LetDefinition functionName typeAnnotation definition

    return (DLetDefinition letDefinition)
    where
        parseFunctionDefinition = indentBlock $ do
            params <- some identifier
            symbol "="
            return (L.IndentMany Nothing (return . EFunction . Function 0 () params . head) parseExpression)
            
parseExpression :: Parser UntypedExpression
parseExpression = EVariable . Variable 0 () <$> identifier

parseTypeAnnotation :: Parser Type
parseTypeAnnotation = colon *> parseType

parseType :: Parser Type
parseType = TConstant <$ symbol "i32"

parse :: FilePath -> Text -> Either ParseError UntypedTopDeclaration
parse file input = runIdentity (runParserT parseTopDeclaration file input)