{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Syntax where

import Data.Text (Text)

import Type
import Name
import NodeId

type BaseProgram = Program ()
type BaseModule = Module ()
type BaseDecl = Decl ()
type BaseExpr = Expr ()

type TypedProgram = Program Type
type TypedModule = Module Type
type TypedDecl = Decl Type
type TypedExpr = Expr Type

type Program x = [Module x]

data Module x = Module
    { modPath :: Namespace
    , imports :: [Import]
    , exports :: [Export]
    , decls   :: [Decl x]
    } deriving (Show)

data Import = Import
    { nodeId     :: !NodeId
    , importPath :: Namespace
    } deriving (Show)

data Export
    = ExportDecl !NodeId Text
    | ExportMod  !NodeId Namespace
    deriving (Show, Eq, Ord)
    
isDeclExport :: Export -> Bool
isDeclExport ExportDecl {} = True
isDeclExport _ = False
isModExport :: Export -> Bool
isModExport ExportMod {} = True
isModExport _ = False

exportedDeclName :: Export -> Text
exportedDeclName (ExportDecl _ name) = name
exportedDeclName _ = undefined
exportedModName :: Export -> Namespace
exportedModName (ExportMod _ ns) = ns
exportedModName _ = undefined

-- FnDecl is parsed then desugared into a DLetDecl
type FnDeclBranch = ([Text], BaseExpr)
data FnDecl = FnDecl
    { nodeId   :: !NodeId
    , name     :: Text
    , annot    :: Maybe Type
    , branches :: [FnDeclBranch]
    } deriving (Show)

data Decl x
    = DData    !NodeId Name [TVar] [(Name, [Type])]
    | DLetDecl !NodeId Name (Maybe Type) (Expr x)
    deriving (Show)

data Expr x
    = ELit     !NodeId x Lit
    | EVar     !NodeId x Name
    | EApp     !NodeId x (Expr x) (Expr x)
    | ELambda  !NodeId x Name (Expr x)
    | ETypeAnn !NodeId x (Expr x) Type
    | ELetExpr !NodeId x Name (Expr x) (Expr x)
    | EIfExpr  !NodeId x (Expr x) (Expr x) (Expr x)
    | EMatch   !NodeId x (Expr x) [(Pattern, Expr x)]
    deriving (Show)

pattern BaseELit id l = ELit id () l
pattern BaseEVar id n = EVar id () n
pattern BaseEApp id f e = EApp id () f e
pattern BaseELambda id p e = ELambda id () p e
pattern BaseETypeAnn id t e = ETypeAnn id () t e
pattern BaseELetExpr id n e b = ELetExpr id () n e b
pattern BaseEIfExpr id c t f = EIfExpr id () c t f
pattern BaseEMatch id e bs = EMatch id () e bs

{-# COMPLETE 
    BaseELit,
    BaseEVar,
    BaseEApp,
    BaseELambda,
    BaseETypeAnn,
    BaseELetExpr,
    BaseEIfExpr,
    BaseEMatch #-}

-- Helper functions
eBinOp :: NodeId -> Name -> BaseExpr -> BaseExpr -> BaseExpr
eBinOp nodeId o a b = BaseEApp nodeId (BaseEApp nodeId (BaseEVar nodeId o) a) b

eUnaOp :: NodeId -> Name -> BaseExpr -> BaseExpr
eUnaOp nodeId o a = BaseEApp nodeId (BaseEVar nodeId o) a

data Lit
    = LInt    Integer
    | LFloat  Double
    | LString Text
    | LChar   Char
    | LBool   Bool
    | LUnit
    deriving (Show)
    
data Pattern
    = PLit Lit
    | PVar Name
    | PWild
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
