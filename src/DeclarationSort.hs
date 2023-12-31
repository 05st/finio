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

import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe
import Data.Generics.Uniplate.Data

import Name
import Syntax

type Sort = State SortState
data SortState = SortState
    { nameMap :: M.Map Name BaseDecl
    , edges :: M.Map Name [Name]
    , reverseEdges :: M.Map Name [Name] -- tranpose graph
    , visited :: S.Set Name
    , stack :: [Name]
    , component :: [Name] -- temporary accumulator
    , nameSet :: S.Set Name
    }

-- Requires decls to be a list of only let declarations.
-- Reverse the result since Kosaraju's algorithm results
-- in a reverse topological sort
sortDeclarations :: [BaseDecl] -> [[BaseDecl]]
sortDeclarations letDecls = reverse (evalState go initSortState)
    where
        letDeclNames = map declName letDecls
        initSortState =
            SortState
            { nameMap = M.fromList (map (\d -> (declName d, d)) letDecls)
            , edges = M.empty
            , reverseEdges = M.empty
            , visited = S.empty
            , stack = []
            , component = []
            , nameSet = S.fromList letDeclNames
            }
        go = do
            mapM_ createEdges letDecls

            mapM_ visit letDeclNames

            modify (\s -> s { visited = S.empty })
            stack <- gets stack
            nameComps <- filter (not . null) <$> traverse (\n -> runAssign n n) stack

            traverse (traverse getDecl) nameComps
        getDecl n = do
            nameMap <- gets nameMap
            case M.lookup n nameMap of
                Nothing -> error "(?) getDecl unreachable case"
                Just d -> return d

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
                curRevEdges = fromMaybe [] (M.lookup b (reverseEdges s))
            put (s {
                edges = M.insert a (b : curEdges) (edges s),
                reverseEdges = M.insert b (a : curRevEdges) (reverseEdges s) })

visit :: Name -> Sort ()
visit n = do
    vs <- gets visited
    if n `S.member` vs
        then return ()
        else do
            modify (\s -> s { visited = S.insert n vs })

            es <- gets (fromMaybe [] . M.lookup n . edges)
            mapM_ visit es

            modify (\s -> s { stack = n : stack s })

runAssign :: Name -> Name -> Sort [Name]
runAssign root n = do
    modify (\s -> s { component = [] })
    assign root n
    gets component

assign :: Name -> Name -> Sort ()
assign root n = do
    vs <- gets visited
    if n `S.member` vs
        then return ()
        else do
            s <- get
            put (s { visited = S.insert n vs, component = n : component s })

            es <- gets (fromMaybe [] . M.lookup n . reverseEdges)
            mapM_ (assign root) es
