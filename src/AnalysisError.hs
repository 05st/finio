{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AnalysisError where

import Data.Text (unpack)
import Data.List
import qualified Data.IntMap as IM

import Error.Diagnose

import Syntax
import Name
import NodeId
import Type

data AnalysisError
    = UndefinedModules            [Import] -- Imports of undefined modules
    | CircularDependency          [String] -- Names of modules in cycle
    | UndefinedIdentifier         String NodeId [String] -- Name of variable + nodeId + any extra hints
    | MultipleDefinitionsImported String NodeId [Import] -- Name of variable + nodeId + imports with definitions
    | MultipleDeclarations        String [NodeId] -- Name of defined variable + nodeIds of declarations
    | ExportedModulesNotImported  [Export] -- Exports of not imported modules
    | ExportedDeclsNotDefined     [Export] -- Exports of undefined declarations
    | TypeMismatch                Type Type NodeId (Maybe NodeId) -- Mismatched types
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

    CircularDependency mods -> do
        let e = err Nothing ("Circular dependencies detected: " ++ intercalate " -> " (mods ++ [head mods])) [] []
        return [addReport def e]
    
    UndefinedIdentifier name nodeId extraHints -> do
        let (pos, src) = extractPositionAndSource nodeId posMap
            e = err Nothing ("Undefined identifier " ++ name) [(pos, This (name ++ " is not in scope"))] extraHints
        
        input <- readFile src
        let diag = addReport (addFile def src input) e
        
        return [diag]

    MultipleDefinitionsImported varName varNodeId imports -> do
        -- It is guaranteed that all imports are located in the same module as the variable (varSrc), read Resolver.hs for more info

        let (varPos, varSrc) = extractPositionAndSource varNodeId posMap
            varMarker = (varPos, This (varName ++ " is used here"))
        
        let importMarkers =
                map (\(Import importNodeId namespace) -> do
                    let modName = showNamespace namespace
                        (impPos, _) = extractPositionAndSource importNodeId posMap
                    (impPos, Where ("Definition found in " ++ modName ++ ", imported here"))
                ) imports
        
        input <- readFile varSrc
        
        let e = err Nothing ("Multiple definitions of variable " ++ varName ++ " found") (varMarker : importMarkers) []
            diag = addReport (addFile def varSrc input) e
        
        return [diag]

    ExportedModulesNotImported exports@(ExportMod sampleNodeId _ : _) -> do
        let (_, src) = extractPositionAndSource sampleNodeId posMap
        
        let (markers, hints) =
                unzip $ map (\(ExportMod nodeId namespace) -> do
                    let modName = showNamespace namespace
                        (expPos, _) = extractPositionAndSource nodeId posMap
                    ((expPos, This (modName ++ " is exported here but never imported")), "Try adding 'import " ++ modName ++ "'")
                ) exports
        
        input <- readFile src
        
        let msg = "Exported module" ++ (if length exports == 1 then [] else "s") ++ " not imported"
            e = err Nothing msg markers hints
            diag = addReport (addFile def src input) e
        
        return [diag]
    
    ExportedDeclsNotDefined exports@(ExportDecl sampleNodeId _ : _) -> do
        let (_, src) = extractPositionAndSource sampleNodeId posMap

        let markers =
                map (\(ExportDecl nodeId declName) -> do
                        let declNameString = unpack declName
                            (expPos, _) = extractPositionAndSource nodeId posMap
                        (expPos, This (declNameString ++ " is exported but never defined"))
                    ) exports
        
        input <- readFile src
        
        let msg = "Exported declaration" ++ (if length exports == 1 then [] else "s") ++ " not defined"
            e = err Nothing msg markers []
            diag = addReport (addFile def src input) e
        
        return [diag]
    
    MultipleDeclarations declName nodeIds@(sampleNodeId : _) -> do
        let (_, src) = extractPositionAndSource sampleNodeId posMap
        
        let labels = map (This . (declName ++)) $ " is defined here" : (repeat " is also defined here")
            positions = map (fst . flip extractPositionAndSource posMap) nodeIds
            markers = zip positions labels
        
        input <- readFile src
        
        let e = err Nothing ("Multiple declarations of " ++ declName) markers []
            diag = addReport (addFile def src input) e
        
        return [diag]
    
    TypeMismatch t1 t2 nodeId1 mNodeId2 -> do
        let (pos1, src) = extractPositionAndSource nodeId1 posMap
        
        let markers =
                case mNodeId2 of
                    Nothing -> [(pos1, This "Type mismatch occured here")]
                    Just nodeId2 ->
                        let (pos2, _) = extractPositionAndSource nodeId2 posMap
                        in [(pos1, This ("This has type " ++ show t1)), (pos2, This ("This has type " ++ show t2))]
        
        input <- readFile src

        let e = err Nothing ("Type mismatch: " ++ show t1 ++ " ~ " ++ show t2) markers []
            diag = addReport (addFile def src input) e
        
        return [diag]
        
    _ -> error "(?) createDiagnostics unreachable case"

extractPositionAndSource :: NodeId -> PositionMap -> (Position, FilePath)
extractPositionAndSource nodeId posMap =
    let pos@(Position _ _ src) = posMap IM.! nodeId
    in (pos, src)
