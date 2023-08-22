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
    } deriving (Show, Functor)

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
data FnDeclDef = FnDeclDef
    { nodeId :: !NodeId
    , pats   :: [Pattern]
    , expr   :: BaseExpr
    } deriving (Show)

data FnDecl = FnDecl
    { nodeId :: !NodeId
    , name   :: Text
    , annot  :: Maybe Type
    , defs   :: [FnDeclDef]
    } deriving (Show)

data Decl x
    = DData      !NodeId Name [Text] [TypeConstr]
    | DLetDecl   !NodeId Name (Maybe Type) (Expr x)
    | DTraitDecl !NodeId Name Text [TraitDef]
    | DImplDecl  !NodeId Name Type [TraitImpl x]
    deriving (Show, Functor)

data TypeConstr
    = TypeConstr !NodeId Text [Type]
    deriving (Show)

data TraitDef
    = TraitDef !NodeId Text Type
    deriving (Show)

data TraitImpl x
    = TraitImpl !NodeId Text (Expr x)
    deriving (Show, Functor)

data Expr x
    = ELit     !NodeId x Lit
    | EVar     !NodeId x Name
    | EApp     !NodeId x (Expr x) (Expr x)
    | ELambda  !NodeId x Name (Expr x)
    | ETypeAnn !NodeId x (Expr x) Type
    | ELetExpr !NodeId x Name (Expr x) (Expr x)
    | EIfExpr  !NodeId x (Expr x) (Expr x) (Expr x)
    | EMatch   !NodeId x (Expr x) [(Pattern, Expr x)]
    | EDoubleColon  !NodeId x Name Text -- For variant constructors, or trait definitions
    | ERecordEmpty  !NodeId x
    | ERecordExtend !NodeId x (Expr x) Text (Expr x)
    deriving (Show, Functor)

pattern BaseELit id l = ELit id () l
pattern BaseEVar id n = EVar id () n
pattern BaseEApp id f e = EApp id () f e
pattern BaseELambda id p e = ELambda id () p e
pattern BaseETypeAnn id t e = ETypeAnn id () t e
pattern BaseELetExpr id n e b = ELetExpr id () n e b
pattern BaseEIfExpr id c t f = EIfExpr id () c t f
pattern BaseEMatch id e bs = EMatch id () e bs
pattern BaseEDoubleColon id t l = EDoubleColon id () t l
pattern BaseERecordEmpty id = ERecordEmpty id ()
pattern BaseERecordExtend id r l e = ERecordExtend id () r l e

{-# COMPLETE 
    BaseELit,
    BaseEVar,
    BaseEApp,
    BaseELambda,
    BaseETypeAnn,
    BaseELetExpr,
    BaseEIfExpr,
    BaseEMatch,
    BaseEDoubleColon,
    BaseERecordEmpty,
    BaseERecordExtend #-}

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
    deriving (Show, Eq)
    
data Pattern
    = PVariant !NodeId Name Text [Name]
    | PLit     !NodeId Lit
    | PVar     !NodeId Name
    | PWild    !NodeId
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

typeOfExpr :: TypedExpr -> Type
typeOfExpr = \case
    ELit _ t _ -> t
    EVar _ t _ -> t
    EApp _ t _ _ -> t
    ELambda _ t _ _ -> t
    ETypeAnn _ t _ _ -> t
    ELetExpr _ t _ _ _ -> t
    EIfExpr _ t _ _ _ -> t
    EMatch _ t _ _ -> t
    EDoubleColon _ t _ _ -> t
    ERecordEmpty _ t -> t
    ERecordExtend _ t _ _ _ -> t

nodeIdOfExpr :: Expr a -> NodeId
nodeIdOfExpr = \case
    ELit n _ _ -> n
    EVar n _ _ -> n
    EApp n _ _ _ -> n
    ELambda n _ _ _ -> n
    ETypeAnn n _ _ _ -> n
    ELetExpr n _ _ _ _ -> n
    EIfExpr n _ _ _ _ -> n
    EMatch n _ _ _ -> n
    EDoubleColon n _ _ _ -> n
    ERecordEmpty n _ -> n
    ERecordExtend n _ _ _ _ -> n

nodeIdOfPat :: Pattern -> NodeId
nodeIdOfPat = \case
    PVariant n _ _ _ -> n
    PLit n _ -> n
    PVar n _ -> n
    PWild n -> n
