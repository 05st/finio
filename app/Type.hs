module Type where

import Data.Text (Text)

data Kind
    = KStar
    | KArrow Kind Kind
    deriving (Show)

data TypeScheme
    = Forall [Kind] Type
    deriving (Show)

data Type
    = TVar TVar
    | TCon TCon
    | TApp Type Type
    deriving (Show)
    
data TVar
    = TV Text Kind
    deriving (Show)

data TCon
    = TC Text Kind
    deriving (Show)
    
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
