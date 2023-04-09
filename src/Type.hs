module Type where

import Data.Text (Text)
import Data.List

import Control.Monad

import Kind

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
    | TCon TCon
    | TApp Type Type
    deriving (Show, Eq)
    
data TVar
    = TV Text Kind
    deriving (Show, Eq)

data TCon
    = TC Text Kind
    deriving (Show, Eq)
    
tArrow :: Type
tArrow = TCon (TC "->" (KArrow KStar (KArrow KStar KStar)))

tInt32 :: Type
tInt32 = TCon (TC "i32" KStar)
tInt64 :: Type
tInt64 = TCon (TC "i64" KStar)

tFloat32 :: Type
tFloat32 = TCon (TC "f32" KStar)
tFloat64 :: Type
tFloat64 = TCon (TC "f64" KStar)

tChar :: Type
tChar = TCon (TC "char" KStar)

tString :: Type
tString = TCon (TC "str" KStar)

tBool :: Type
tBool = TCon (TC "bool" KStar)

tUnit :: Type
tUnit = TCon (TC "unit" KStar)

instance HasKind TVar where
    kind (TV _ k) = k

instance HasKind TCon where
    kind (TC _ k) = k

instance HasKind Type where
    kind (TCon t) = kind t
    kind (TVar t) = kind t
    kind (TApp t _) =
        case kind t of
            (KArrow _ k) -> k
            KStar -> error "(?) TApp t, t had KStar kind"
