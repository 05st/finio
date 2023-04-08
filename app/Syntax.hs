{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Syntax where

import Data.Text (Text)
import qualified Data.IntMap as IM

import Type

type NodeId = Int

type SpanMap = IM.IntMap Span -- NodeId -> Span
data Span
    = Span FilePath (Int, Int) (Int, Int)
    deriving (Show)

type BaseDecl = Decl ()
type BaseExpr = Expr ()
type TypedDecl = Decl Type
type TypedExpr = Expr Type

-- FnDecl is parsed then desugared into a DLetDecl
type FnDeclBranch = ([Text], BaseExpr)
data FnDecl = FnDecl
    { nodeId :: NodeId
    , name :: Text
    , annot :: Maybe Type
    , branches :: [FnDeclBranch]
    } deriving (Show)

data Decl x
    = DTrait
    | DImpl
    | DLetDecl NodeId Text (Maybe Type) (Expr x)
    deriving (Show)

data Expr x
    = ELit     NodeId x Lit
    | EVar     NodeId x Text
    | EApp     NodeId x (Expr x) (Expr x)
    | EBinOp   NodeId x Text (Expr x) (Expr x)
    | EUnaOp   NodeId x Text (Expr x)
    | ELambda  NodeId x [Text] (Expr x)
    | ETypeAnn NodeId x (Expr x) Type
    | ELetExpr NodeId x Text (Expr x) (Expr x)
    | EIfExpr  NodeId x (Expr x) (Expr x) (Expr x)
    deriving (Show)
    
pattern BaseELit id l = ELit id () l
pattern BaseEVar id n = EVar id () n
pattern BaseEApp id f e = EApp id () f e
pattern BaseEBinOp id o a b = EBinOp id () o a b
pattern BaseEUnaOp id o a = EUnaOp id () o a
pattern BaseELambda id p e = ELambda id () p e
pattern BaseETypeAnn id t e = ETypeAnn id () t e
pattern BaseELetExpr id n e b = ELetExpr id () n e b
pattern BaseEIfExpr id c t f = EIfExpr id () c t f

data Lit
    = LInt    Integer
    | LFloat  Double
    | LString Text
    | LChar   Char
    | LBool   Bool
    | LUnit
    deriving (Show)
    
data Assoc
    = ALeft
    | ARight
    | ANone
    | APrefix
    | APostfix
    deriving (Show)

data OperatorDef = OperatorDef
    { assoc :: Assoc
    , prec  :: Integer
    , oper  :: Text
    } deriving (Show)
