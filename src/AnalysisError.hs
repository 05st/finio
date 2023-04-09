{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AnalysisError where

import Data.Text (unpack)
import Data.List
import qualified Data.IntMap as IM

import Error.Diagnose

import Syntax
import Namespace
import NodeId

data AnalysisError
    = UndefinedModules           [Import] -- Imports of undefined modules
    | CyclicDependency           [String] -- Names of modules in cycle
    | UndefinedIdentifier        String NodeId -- Name of variable + nodeId
    | MultipleDefinitions        String NodeId [Import] -- Name of variable + nodeId + imports with definitions
    | ExportedModulesNotImported [Export] -- Exports of not imported modules
    | ExportedDeclsNotDefined    [Export] -- Exports of undefined declarations
    deriving (Show)

createDiagnostics :: PositionMap -> AnalysisError -> IO [Diagnostic String]
createDiagnostics posMap = \case
    UndefinedModules imports -> traverse (\(Import nodeId namespace) -> do
            let name = showNamespace namespace
                (pos, src) = extractPositionAndSource nodeId posMap
                e = err Nothing ("Undefined module " ++ name) [(pos, This ("Undefined module " ++ name ++ " was imported here"))] []
            
            input <- readFile src
            let diag = addReport (addFile def src input) e
            
            return diag
        ) imports

    CyclicDependency mods -> do
        let e = err Nothing ("Circular dependencies detected: " ++ intercalate " -> " (mods ++ [head mods])) [] []
        return [addReport def e]
    
    UndefinedIdentifier name nodeId -> do
        let (pos, src) = extractPositionAndSource nodeId posMap
            e = err Nothing ("Undefined identifier " ++ name) [(pos, This ("Undefined identifier " ++ name ++ " is used here"))] []
        
        input <- readFile src
        let diag = addReport (addFile def src input) e
        
        return [diag]

    MultipleDefinitions varName varNodeId imports -> do
        -- It is guaranteed that all imports are located in the same module as the variable (varSrc), read Resolver.hs for more info

        let (varPos, varSrc) = extractPositionAndSource varNodeId posMap
            varMarker = (varPos, This (varName ++ " is used here"))
        
        importMarkers <- traverse (\(Import importNodeId namespace) -> do
                let modName = showNamespace namespace
                    (impPos, _) = extractPositionAndSource importNodeId posMap
                return (impPos, Where ("Definition found in " ++ modName ++ ", imported here"))
            ) imports
        
        input <- readFile varSrc
        
        let e = err Nothing ("Multiple definitions of variable " ++ varName ++ " found") (varMarker : importMarkers) []
            diag = addReport (addFile def varSrc input) e
        
        return [diag]

    ExportedModulesNotImported exports@(ExportMod sampleNodeId _ : _) -> do
        let (_, src) = extractPositionAndSource sampleNodeId posMap
        
        (markers, hints) <- unzip <$> traverse (\(ExportMod nodeId namespace) -> do
                let modName = showNamespace namespace
                    (expPos, _) = extractPositionAndSource nodeId posMap
                return ((expPos, This (modName ++ " is exported here but never imported")), "Try adding 'import " ++ modName ++ "'")
            ) exports
        
        input <- readFile src
        
        let msg = "Exported module" ++ (if length exports == 1 then [] else "s") ++ " not imported"
            e = err Nothing msg markers hints
            diag = addReport (addFile def src input) e
        
        return [diag]
    
    ExportedDeclsNotDefined exports@(ExportDecl sampleNodeId _ : _) -> do
        let (_, src) = extractPositionAndSource sampleNodeId posMap

        markers <- traverse (\(ExportDecl nodeId declName) -> do
                let declNameString = unpack declName
                    (expPos, _) = extractPositionAndSource nodeId posMap
                return (expPos, This (declNameString ++ " is exported but never defined"))
            ) exports
        
        input <- readFile src
        
        let msg = "Exported declaration" ++ (if length exports == 1 then [] else "s") ++ " not defined"
            e = err Nothing msg markers []
            diag = addReport (addFile def src input) e
        
        return [diag]
        
    _ -> error "(?) createDiagnostics unreachable case"

extractPositionAndSource :: NodeId -> PositionMap -> (Position, FilePath)
extractPositionAndSource nodeId posMap =
    let pos@(Position _ _ src) = posMap IM.! nodeId
    in (pos, src)
