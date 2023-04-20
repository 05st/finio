module Substitution where

import qualified Data.Map as M
import qualified Data.Set as S

import Type

type Subst = M.Map TVar Type

class Substitutable t where
    apply :: Subst -> t -> t
    ftv :: t -> S.Set TVar

instance Substitutable Type where
    apply :: Subst -> Type -> Type
    apply s t@(TVar u) = M.findWithDefault t u s
    apply s (TApp a b) = TApp (apply s a) (apply s b)
    apply _ t@TCon {} = t

    ftv :: Type -> S.Set TVar
    ftv (TVar u) = S.singleton u
    ftv (TApp a b) = ftv a `S.union` ftv b
    ftv TCon {} = S.empty

instance Substitutable TypeScheme where
    apply :: Subst -> TypeScheme -> TypeScheme
    apply s (Forall vs t) = Forall vs (apply (foldr M.delete s vs) t)

    ftv :: TypeScheme -> S.Set TVar
    ftv (Forall vs t) = ftv t `S.difference` S.fromList vs

instance Substitutable a => Substitutable [a] where
    apply :: Substitutable a => Subst -> [a] -> [a]
    apply = map . apply
    
    ftv :: Substitutable a => [a] -> S.Set TVar
    ftv = foldr (S.union . ftv) S.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 =
    let s = M.union s1 s2
    in apply s <$> s

nullSubst :: Subst
nullSubst = M.empty
