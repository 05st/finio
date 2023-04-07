{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Syntax where

import Data.Text (Text)

import Type

type BaseDecl = Decl ()
type BaseExpr = Expr ()

-- FnDecl is parsed then desugared into a DLetDecl
type FnDeclBranch = ([Text], BaseExpr)
data FnDecl = FnDecl
    { name :: Text
    , annot :: Maybe Type
    , branches :: [FnDeclBranch]
    } deriving (Show)

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
    | ETypeAnn x (Expr x) Type
    | ELetExpr x (LetExpr x)
    | EIfExpr  x (IfExpr x)
    deriving (Show)

pattern BaseELit l = ELit () l
pattern BaseEVar n = EVar () n
pattern BaseEApp f e = EApp () f e
pattern BaseELambda ps e = ELambda () ps e
pattern BaseETypeAnn t e = ETypeAnn () t e
pattern BaseELetExpr x = ELetExpr () x
pattern BaseEIfExpr x = EIfExpr () x

pattern EBinOp t a b = BaseEApp (BaseEApp (BaseEVar t) a) b
pattern EUnaOp t a = BaseEApp (BaseEVar t) a

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
    
data IfExpr x = IfExpr
    { cond  :: Expr x
    , onTrue  :: Expr x
    , onFalse :: Expr x
    } deriving (Show)

data Assoc
    = ALeft
    | ARight
    | ANone
    | APrefix
    | APostfix
    deriving (Show)

data OperatorDef = OperatorDef
    { assoc :: Assoc
    , prec :: Integer
    , oper :: Text
    } deriving (Show)
