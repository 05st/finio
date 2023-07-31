module Desugar where

import Data.Text (Text, pack)
import Data.List

import Control.Monad

import Lexer
import Syntax
import Name

import Debug.Trace

data PatTree
    = Nested Pattern [PatTree]
    | Expr Pattern BaseExpr
    deriving (Show)
 
-- Compare patterns while ignoring nodeId
patCompare :: Pattern -> Pattern -> Bool
patCompare (PVariant _ a b c) (PVariant _ a' b' c') = a == a' && b == b' && c == c'
patCompare (PLit _ a) (PLit _ a') = a == a'
patCompare (PVar _ a) (PVar _ a' ) = a == a'
patCompare (PWild _) (PWild _) = True
patCompare _ _ = False

checkNode :: Pattern -> PatTree -> Bool
checkNode p (Nested p' _) = p `patCompare` p'
checkNode p (Expr p' _) = p `patCompare` p'

getChildren :: PatTree -> [PatTree]
getChildren (Nested _ cs) = cs
getChildren (Expr _ _) = []

-- Turns a flat list of patterns (parameters) to a linear tree 
expandFlatParams :: [Pattern] -> BaseExpr -> PatTree
expandFlatParams (p : []) expr = Expr p expr
expandFlatParams (p : ps) expr = Nested p [expandFlatParams ps expr]
expandFlatParams [] _ = error "(?) expandFlatParams called with empty list"

-- Groups all children of matching pattern as children of the first one
groupTrees :: [PatTree] -> [PatTree]
-- Once we encounter an Expr node we can stop since all nodes at that depth should be guaranteed to be Expr nodes
groupTrees a@((Expr _ _) : _) = a
groupTrees ((Nested refPat initChildren) : rest) = do
    let (patMatches, rest') = partition (checkNode refPat) rest
        newChildren = initChildren ++ concatMap getChildren patMatches
    (Nested refPat newChildren) : groupTrees rest'
groupTrees [] = []

-- Recursively calls groupTrees on every depth
groupChildren :: PatTree -> PatTree
groupChildren (Nested p cs) = Nested p (map groupChildren (groupTrees cs))
groupChildren t@(Expr _ _) = t

constructMatch :: Int -> [PatTree] -> BaseExpr
constructMatch depth trees = do
    let matchBranches = map constructMatchBranch trees
    BaseEMatch 0 (BaseEVar 0 (paramNames !! depth)) matchBranches
    where
        constructMatchBranch (Expr p e) = (p, e)
        constructMatchBranch (Nested p cs) = (p, constructMatch (depth + 1) cs)

paramNames :: [Name]
paramNames = map (unqualified . (pack . ('_':))) ([1..] >>= flip replicateM ['a'..'z']) 

desugarFnDecl :: FnDecl -> Parser BaseDecl
desugarFnDecl (FnDecl nodeId fnName annot defs) = do
    let arityNotMatch = filter (\d -> length (pats d) /= arity) defs
    unless (null arityNotMatch) $ do
        -- TODO report the actual nodeId/position of the FnDeclDef that doesnt have the correct arity
        fail "all definitions in fn decl should have same arity"
    return $ DLetDecl nodeId (unqualified fnName) annot lambda
    where
        arity = (length . pats . head) defs
        trees = map (\(FnDeclDef _ pats expr) -> expandFlatParams pats expr) defs
        grouped = map groupChildren (groupTrees trees)
        matchExpr = constructMatch 0 grouped
        lambda = foldr (BaseELambda nodeId) matchExpr (take arity paramNames)
