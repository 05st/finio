{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Toposort where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import AnalysisError
import Syntax hiding (FnDecl(nodeId))
import Name

type Sort = ExceptT AnalysisError (State SortState)
data SortState = CheckState
    { edges :: M.Map Namespace [Import]
    , visited :: S.Set Namespace
    
    , modMap :: M.Map Namespace BaseModule
    , sorted :: [BaseModule]
    } deriving (Show)

sortProgram :: BaseProgram -> Either AnalysisError BaseProgram
sortProgram modules =
    case runState (runExceptT (sortModules (map modPath modules))) initSortState of
        (Left err, _) -> Left err
        (_, state) -> Right (reverse (sorted state))
    where
        initSortState = CheckState { edges = initEdges, visited = initVisited, modMap = initModMap, sorted = [] }
        initEdges = M.fromList (map (modPath &&& imports) modules)
        initVisited = S.empty
        initModMap = M.fromList (map (modPath &&& id) modules)

sortModules :: [Namespace] -> Sort ()
sortModules [] = return ()
sortModules (mod : mods) = do
    dfs [] mod
    sortModules mods
    where
        visit mod = modify (\s -> s { visited = S.insert mod (visited s) })

        finalize mod = do
            modMap <- gets modMap
            modify (\s -> s { sorted = (modMap M.! mod) : sorted s })

        dfs cycle mod = do
            visiteds <- gets visited
            when (S.notMember mod visiteds) $ do
                visit mod
                edgesMap <- gets edges

                let modEdges = M.findWithDefault [] mod edgesMap
                    modEdgesPaths = map importPath modEdges
                    undefinedImports = filter ((`S.notMember` M.keysSet edgesMap) . importPath) modEdges

                unless (null undefinedImports) -- Verify imported modules exist
                    $ throwError (UndefinedModules undefinedImports)

                when (any (`elem` cycle) modEdgesPaths) -- Check for any cycles
                    $ throwError (CircularDependency (map showNamespace (reverse (mod : cycle))))
                
                mapM_ (dfs (mod : cycle)) modEdgesPaths

                finalize mod
