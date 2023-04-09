module Substitution where

import Data.List

import Type

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
