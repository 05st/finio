{-# LANGUAGE DuplicateRecordFields #-}

module Syntax where

import Data.Text (Text)
import qualified Data.Text as T

import Type

data TopDeclaration a
    = TDTrait
    | TDDeclaration (Declaration a)
    deriving (Show)

data Declaration a
    = DLetDefinition (LetDefinition a)
    | DStatement (Statement a)
    deriving (Show)

data LetDefinition a = LetDefinition
    { name :: Text
    , annotation :: Maybe Type
    , body :: Expression a
    } deriving (Show)

data Statement a
    = SReturn (Expression a)
    | SExpression (Expression a)
    deriving (Show)

data Expression a
    = ELiteral (Literal a)
    | EVariable (Variable a)
    | EFunction (Function a)
    | EFunctionCall (FunctionCall a)
    | EAnnotated (Expression a)
    deriving (Show)

data LiteralKind
    = LInteger Integer
    | LFloat Double
    deriving (Show)

data Literal a = Literal
    { location :: Integer
    , typ :: a
    , kind :: LiteralKind
    } deriving (Show)

data Variable a = Variable
    { location :: Integer
    , typ :: a
    , name :: Text
    } deriving (Show)

data Function a = Function
    { location :: Integer
    , typ :: a
    , parameters :: [Text]
    , body :: Expression a
    } deriving (Show)

data FunctionCall a = FunctionCall
    { location :: Integer
    , typ :: a
    } deriving (Show)

type UntypedTopDeclaration = TopDeclaration ()
type UntypedDeclaration = Declaration ()
type UntypedStatement = Statement ()
type UntypedExpression = Expression ()

type TypedTopDeclaration = TopDeclaration Type
type TypedDeclaration = Declaration Type
type TypedStatement = Statement Type
type TypedExpression = Expression Type