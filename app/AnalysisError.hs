module AnalysisError where

import Data.List
import qualified Data.IntMap as IM

import Error.Diagnose

import Syntax

data AnalysisError
    = UndefinedModulesError [(String, NodeId)] -- Name of imported module + import statement nodeId
    | CyclicDependencyError [String] -- Names of modules in cycle
    deriving (Show)

createDiagnostics :: PositionMap -> AnalysisError -> IO [Diagnostic String]
createDiagnostics posMap = \case
    UndefinedModulesError errs -> traverse (\(name, nodeId) -> do
            let pos@(Position _ _ src) = posMap IM.! nodeId
                e = err Nothing "Attempt to import undefined module" [(pos, This ("Module " ++ name ++ " does not exist"))] []
            
            input <- readFile src
            let diag = addReport (addFile def src input) e
            
            return diag
        ) errs

    CyclicDependencyError mods -> do
        let e = err Nothing ("Circular dependencies detected: " ++ intercalate " -> " (mods ++ [head mods])) [] []
        return [addReport def e]
