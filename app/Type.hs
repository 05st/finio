module Type where

import Data.Text (Text)
import Data.List

import Control.Monad

-- Kinds

class HasKind t where
    kind :: t -> Kind

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

data Kind
    = KStar
    | KArrow Kind Kind
    deriving (Show, Eq)


-- Types

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


-- Traits

data Predicate
    = IsIn Text Type
    deriving (Show, Eq)

data Qualified t
    = [Predicate] :=> t
    deriving (Show)

type Impl = Qualified Predicate

type Trait = ([Text], [Impl])

data TraitEnv = TraitEnv
    { traits :: Text -> Maybe Trait
    , defaults :: [Type]
    }

lift :: MonadFail m => (Type -> Type -> m a) -> Predicate -> Predicate -> m a
lift m (IsIn i t) (IsIn i' t')
    | i == i' = m t t'
    | otherwise = fail "Classes differ"

super :: TraitEnv -> Text -> [Text]
super env i =
    case traits env i of
        Just (is, _) -> is
        Nothing -> []

impls :: TraitEnv -> Text -> [Impl]
impls env i =
    case traits env i of
        Just (_, is) -> is
        Nothing -> []

entail :: TraitEnv -> [Predicate] -> Predicate -> Bool
entail env ps p =
    any (p `elem`) (map (bySuper env) ps) ||
        case byImpl env p of
            Nothing -> False
            Just quals -> all (entail env ps) quals

bySuper :: TraitEnv -> Predicate -> [Predicate]
bySuper env p@(IsIn i t) =
    p : concat [bySuper env (IsIn i' t) | i' <- super env i]

byImpl :: TraitEnv -> Predicate -> Maybe [Predicate]
byImpl env p@(IsIn i _) = msum (map tryImpl (impls env i))
    where
        tryImpl (ps :=> h) = do
            u <- matchPred h p
            Just (map (apply u) ps)

matchPred :: Predicate -> Predicate -> Maybe Subst
matchPred = lift match

match :: MonadFail m => Type -> Type -> m [(TVar, Type)]
match (TApp l r) (TApp l' r') = do
    sl <- match l l'
    sr <- match r r'
    merge sl sr
match (TVar u) t
    | kind u == kind t = return (u +-> t)
match (TCon a) (TCon b) | a == b = return mempty
match _ _ = undefined -- Type mismatch



-- Substitutions

type Subst = [(TVar, Type)]

(+->) :: TVar -> Type -> Subst
u +-> t = [(u, t)]

class Substitutable t where
    apply :: Subst -> t -> t
    tvs   :: t -> [TVar]

instance Substitutable Type where
    apply s (TVar u) =
        case lookup u s of
            Just t -> t
            Nothing -> TVar u
    apply s (TApp a b) = TApp (apply s a) (apply s b)
    apply _ t@TCon {} = t

    tvs (TVar u) = [u]
    tvs (TApp a b) = tvs a ++ tvs b
    tvs TCon {} = []
    
instance Substitutable t => Substitutable (Qualified t) where
    apply s (ps :=> t) = apply s ps :=> apply s t
    tvs (ps :=> t) = tvs ps ++ tvs t

instance Substitutable Predicate where
    apply s (IsIn i t) = IsIn i (apply s t)
    tvs (IsIn _ t) = tvs t

instance Substitutable a => Substitutable [a] where
    apply s = map (apply s)
    tvs = nub . concatMap tvs

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

merge :: MonadFail m => [(TVar, Type)] -> [(TVar, Type)] -> m [(TVar, Type)]
merge s1 s2 = if agree then return (s1 ++ s2) else fail "Merge failed"
    where
        agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) (map fst s1 `intersect` map fst s2)
