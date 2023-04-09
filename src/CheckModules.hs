{-# OPTIONS_GHC -Wno-name-shadowing #-}

module CheckModules where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import AnalysisError
import Syntax hiding (FnDecl(nodeId))
import Namespace

type Check = ExceptT AnalysisError (State CheckState)
data CheckState = CheckState
    { edges :: M.Map Namespace [Import]
    , visited :: S.Set Namespace
    } deriving (Show)

checkModules :: BaseProgram -> Maybe AnalysisError
checkModules modules =
    case evalState (runExceptT (checkProgram modPaths)) (CheckState { edges = initEdges, visited = initVisited }) of
        Left err -> Just err
        _ -> Nothing
    where
        modPaths = map modPath modules
        initEdges = M.fromList (map (modPath &&& imports) modules)
        initVisited = S.empty

checkProgram :: [Namespace] -> Check ()
checkProgram [] = return ()
checkProgram (mod : mods) = do
    visiteds <- gets visited
    when (S.notMember mod visiteds) (dfs [] mod)
    checkProgram mods
    where
        visit m = modify (\s -> s { visited = S.insert m (visited s) })
        dfs cycle mod = do
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
