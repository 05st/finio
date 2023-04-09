module Trait where

import Data.Text (Text)

import Control.Monad

import Type
import Kind
import Substitution

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
