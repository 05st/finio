{-# LANGUAGE DuplicateRecordFields #-}

module Syntax where

import qualified Data.Text as T
import Type

data TopDeclaration a
    = TDTrait
    | TDDeclaration (Declaration a)

data Declaration a
    = DLetDefinition (LetDefinition a)
    | DStatement (Statement a)

data LetDefinition a = LetDefinition
    { name :: T.Text
    , annotation :: Maybe Type
    }

data Statement a
    = SReturn (Expression a)
    | SExpression (Expression a)

data Expression a
    = ELiteral (Literal a)
    | EVariable (Variable a)
    | EFunction (Function a)
    | EFunctionCall (FunctionCall a)
    | EAnnotated (Expression a)

data LiteralKind
    = LInteger Integer
    | LFloat Double

data Literal a = Literal
    { kind :: LiteralKind
    , location :: Integer
    , typ :: a
    }

data Variable a = Variable
    { name :: T.Text
    , location :: Integer
    , typ :: a
    }

data Function a = Function
    { location :: Integer
    , typ :: a
    }

data FunctionCall a = FunctionCall
    { location :: Integer
    , typ :: a
    }
