module Type where

import Data.Text (Text, unpack)
import Data.List

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
    = Forall [TVar] Type
    deriving (Show)

data Type
    = TVar          TVar
    | TCon          !NodeId TCon
    | TApp          Type Type
    | TRecordExtend Text Type Type
    | TRecordEmpty  
    deriving (Eq)
    
data TVar
    = TV Text Kind
    deriving (Eq, Ord)

getTVText :: TVar -> Text
getTVText (TV t _) = t

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
    kind a@(TApp t _) =
        case kind t of
            (KArrow _ k) -> k
            KStar -> error ("(?) TApp t, t had KStar kind (" ++ show a ++ ")")
    kind (TRecordExtend {}) = KStar
    kind (TRecordEmpty {}) = KStar

instance Show Type where
    show (TVar tv) = show tv
    show (TCon _ (TC name _)) = unpack (getIdentifier name)
    show (TApp (TApp (TCon _ (TC (Name [] "->") _)) a) b) = '(' : show a ++ " -> " ++ show b ++ ")"
    show (TApp a b) = "(" ++ show a ++ " " ++ show b ++ ")"

    show t@TRecordExtend {} =
        let items = collect t
        in "{" ++ intercalate ", " [unpack l ++ " : " ++ show tt | (l, tt) <- items] ++ "}"
        where
            collect (TRecordExtend l tt e) = (l, tt) : collect e
            collect TRecordEmpty = []
            collect _ = undefined

    show TRecordEmpty = "{}"

instance Show TVar where
    show (TV ident _) = unpack ident
