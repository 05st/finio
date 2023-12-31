{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module DeclarationSort where

{-
Constructs a graph based on how the let declarations in one module
depend on one another. Performs Kosaraju's algorithm for finding
strongly connected components to find the smallest groups of mutually
recursive declarations. Additionally, the result is topologically
sorted (in reverse) as a byproduct of Kosaraju's algorithm (the
topological sorting is also required for further semantic analysis).
-}

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe
import Data.Generics.Uniplate.Data

import Name
import Syntax

import Debug.Trace

type Sort = State SortState
data SortState = SortState
    { nameMap :: M.Map Name BaseDecl
    , edges :: M.Map Name [Name]
    , reverseEdges :: M.Map Name [Name] -- tranpose graph
    , visited :: S.Set Name
    , stack :: [Name]
    , nameSet :: S.Set Name
    }

-- Requires decls to be a list of only let declarations.
sortDeclarations :: [BaseDecl] -> [[BaseDecl]]
sortDeclarations letDecls = evalState go initSortState
    where
        letDeclNames = map declName letDecls
        initSortState =
            SortState
            { nameMap = M.fromList (map (\d -> (declName d, d)) letDecls)
            , edges = M.empty
            , reverseEdges = M.empty
            , visited = S.empty
            , stack = []
            , nameSet = S.fromList letDeclNames
            }
        go = do
            mapM_ createEdges letDecls
            mapM_ firstDFS letDeclNames
            undefined

createEdges :: BaseDecl -> Sort ()
createEdges decl = do
    names <- gets nameSet

    let varNames = map (\(EVar _ _ n) -> n) (gatherVarExprs decl)
        targets = filter (\n -> (n `S.member` names) && (n /= declName decl)) varNames

    mapM_ (addEdge (declName decl)) targets
    where
        gatherVarExprs (DLetDecl _ _ _ e) = [x | x@(EVar {}) <- universe e]
        gatherVarExprs _ = error "(?) unreachable gatherVarExprs case"

        addEdge a b = do
            s <- get
            let curEdges = fromMaybe [] (M.lookup a (edges s))
                curRevEdges = fromMaybe [] (M.lookup b (edges s))
            put (s {
                edges = M.insert a (b : curEdges) (edges s),
                reverseEdges = M.insert b (a : curRevEdges) (reverseEdges s) })

firstDFS :: Name -> Sort ()
firstDFS n = do
    vs <- gets visited
    when (n `S.member` vs) $ return ()
    trace "LMAO" $ return ()

    modify (\s -> s { visited = S.insert n (visited s) })

    es <- gets (fromMaybe [] . M.lookup n . edges)
    mapM_ firstDFS es

    modify (\s -> s { stack = n : stack s })

secondDFS :: Name -> Sort ()
secondDFS = undefined
