module Type where

import Data.Text (Text)

import Kind
import Name
import NodeId

data Predicate
    = IsIn Text Type
    deriving (Show, Eq)

data Qualified t
    = [Predicate] :=> t
    deriving (Show)

data TypeScheme
    = Forall [Kind] (Qualified Type)
    deriving (Show)

data Type
    = TVar TVar
    | TCon !NodeId TCon
    | TApp Type Type
    deriving (Show, Eq)
    
data TVar
    = TV Text Kind
    deriving (Show, Eq)

data TCon
    = TC Name Kind
    deriving (Show, Eq)
    
primTypes :: [Text]
primTypes = ["->", "i32", "i64", "f32", "f64", "char", "str", "bool", "unit"]

tArrow :: NodeId -> Type
tArrow = flip TCon (TC (unqualified "->") (KArrow KStar (KArrow KStar KStar)))

tInt32 :: NodeId -> Type
tInt32 = flip TCon (TC (unqualified "i32") KStar)
tInt64 :: NodeId -> Type
tInt64 = flip TCon (TC (unqualified "i64") KStar)

tFloat32 :: NodeId -> Type
tFloat32 = flip TCon (TC (unqualified "f32") KStar)
tFloat64 :: NodeId -> Type
tFloat64 = flip TCon (TC (unqualified "f64") KStar)

tChar :: NodeId -> Type
tChar = flip TCon (TC (unqualified "char") KStar)

tString :: NodeId -> Type
tString = flip TCon (TC (unqualified "str") KStar)

tBool :: NodeId -> Type
tBool = flip TCon (TC (unqualified "bool") KStar)

tUnit :: NodeId -> Type
tUnit = flip TCon (TC (unqualified "unit") KStar)

instance HasKind TVar where
    kind (TV _ k) = k

instance HasKind TCon where
    kind (TC _ k) = k

instance HasKind Type where
    kind (TCon _ t) = kind t
    kind (TVar t) = kind t
    kind (TApp t _) =
        case kind t of
            (KArrow _ k) -> k
            KStar -> error "(?) TApp t, t had KStar kind"
