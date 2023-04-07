module Syntax where

import Data.Text (Text)

import Type

data Decl x
    = DTrait
    | DImpl
    | DLetDecl (LetDecl x)
    deriving (Show)

data Expr x
    = ELit     x Lit
    | EVar     x Text
    | EApp     x (Expr x) (Expr x)
    | ELambda  x [Text] (Expr x)
    | ETypeAnn x Type (Expr x)
    | ELetExpr x (LetExpr x)
    deriving (Show)

data Lit
    = LInt    Integer
    | LFloat  Double
    | LString Text
    | LChar   Char
    | LBool   Bool
    | LUnit
    deriving (Show)
    
data LetDecl x = LetDecl
    { name  :: Text
    , annot :: Maybe Type
    , expr  :: Expr x
    } deriving (Show)
    
data LetExpr x = LetExpr
    { name :: Text
    , expr :: Expr x
    , body :: Expr x
    } deriving (Show)
