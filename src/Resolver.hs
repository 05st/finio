module Resolver where

import Data.Text (Text, pack, unpack)
import qualified Data.Set as S
import qualified Data.Map as M

import Data.Maybe

import Control.Arrow

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import AnalysisError
import Syntax

type Resolve = ExceptT AnalysisError (ReaderT Namespace (State ResolverState))
data ResolverState = ResolverState
    { exportsMap :: M.Map Namespace (S.Set Export)
    , nameSet :: S.Set [Text]
    , modImports :: [Import]
    , modNamespace :: Namespace
    , tempScopeCount :: Int
    } deriving (Show)

-- Utility function for creating new scope names
tempScope :: Resolve Text
tempScope = do
    s <- get
    put (s { tempScopeCount = tempScopeCount s + 1 })
    return . pack . ('_':) $ ([1..] >>= flip replicateM ['a'..'z']) !! tempScopeCount s

resolveProgram :: BaseProgram -> Either AnalysisError BaseProgram
resolveProgram modules = evalState (runReaderT (runExceptT (traverse resolveModule modules)) []) initResolverState
    where
        initResolverState =
            ResolverState { exportsMap = initExportsMap, nameSet = S.empty, modImports = [], modNamespace = [], tempScopeCount = 0}
        initExportsMap =
            M.fromList (map (modPath &&& (S.fromList . exports)) modules)

resolveModule :: BaseModule -> Resolve BaseModule
resolveModule m = do
    s <- get

    let modPrefix = modPath m
        declNames = map getDeclName (decls m)
        initNameSet = S.fromList (map (: modPrefix) declNames)
    
    put (s { nameSet = initNameSet, modImports = imports m, modNamespace = modPrefix })

    resolvedDecls <- local (++ modPrefix) (traverse resolveDecl (decls m))
    
    put s

    return (m { decls = resolvedDecls })
    
    where
        getDeclName (DLetDecl _ name _ _) = name
        getDeclName _ = undefined

resolveDecl :: BaseDecl -> Resolve BaseDecl
resolveDecl (DLetDecl nodeId varName typeAnn expr) = do
    DLetDecl nodeId varName typeAnn <$> resolveExpr expr
resolveDecl _ = undefined

resolveExpr :: BaseExpr -> Resolve BaseExpr
resolveExpr (BaseELit nodeId lit) =
    return (BaseELit nodeId lit)

resolveExpr (BaseEVar nodeId varName) = do
    namespace <- resolveName nodeId varName
    return (EVar nodeId () namespace varName)

resolveExpr (BaseEApp nodeId a b) =
    BaseEApp nodeId <$> resolveExpr a <*> resolveExpr b

resolveExpr (BaseELambda nodeId paramName expr) = do
    curScope <- ask
    tmp <- tempScope
    
    s <- get
    put (s { nameSet = S.insert (curScope ++ tmp : [paramName]) (nameSet s) })

    resolvedExpr <- local (++ [tmp]) (resolveExpr expr)

    return (BaseELambda nodeId paramName resolvedExpr)

resolveExpr (BaseETypeAnn nodeId expr typ) = do
    resolvedExpr <- resolveExpr expr
    undefined
    return (BaseETypeAnn nodeId resolvedExpr typ)

resolveExpr (BaseELetExpr nodeId varName expr body) = do
    resolvedExpr <- resolveExpr expr

    curScope <- ask
    tmp <- tempScope

    s <- get
    put (s { nameSet = S.insert (curScope ++ tmp : [varName]) (nameSet s) })
    
    resolvedBody <- local (++ [tmp]) (resolveExpr body)
    
    return (BaseELetExpr nodeId varName resolvedExpr resolvedBody)

resolveExpr (BaseEIfExpr nodeId c a b) =
    BaseEIfExpr nodeId <$> resolveExpr c <*> resolveExpr a <*> resolveExpr b

resolveExpr (BaseEMatch nodeId expr branches) = do
    undefined

-- Finds the namespace
resolveName :: NodeId -> Text -> Resolve Namespace
resolveName varNodeId n = do
    modNamespace <- gets modNamespace
    
    -- Check module decls first
    namesSet <- gets nameSet
    if (modNamespace ++ [n]) `S.member` namesSet
        then return modNamespace
        else ask >>= resolveRecursive n
    
    where
        resolveRecursive name curNamespace = do
            nameSet <- gets nameSet
            modNamespace <- gets modNamespace
            
            let qualifiedName = curNamespace ++ [name]
            if qualifiedName `S.member` nameSet
                then return curNamespace
                else if curNamespace /= modNamespace -- We've checked every scope up the module's decls (which were checked earlier)
                    then resolveRecursive name (init curNamespace)
                    else do -- Check imports
                        modImports <- gets modImports
                        passedImports <- concat <$> traverse gatherAllParentImports modImports
                        let allImports = modImports ++ passedImports

                        found <- concat <$> traverse (checkExport name) allImports
                        case found of
                            [] -> throwError (UndefinedVariable (unpack name) varNodeId) -- Undefined

                            [Import _ namespace] -> return namespace

                            many -> throwError (MultipleDefinitions (unpack name) varNodeId many) -- Multiple definitions
                            -- Since nodeId is passed in modExportToImport, it is guaranteed that the nodeIds contain the same source in all Imports of 'many'
                            
        checkExport name imp@(Import _ namespace) = do
            exportsMap <- gets exportsMap
            let exports = exportsMap M.! namespace
            let declExports = S.map exportedDeclName (S.filter (not . isModExport) exports)

            if name `S.member` declExports
                then return [imp]
                else return []

-- Recursively gets all imports (including passed-through imports)
gatherAllParentImports :: Import -> Resolve [Import]
gatherAllParentImports (Import nodeId importPath) = do
    exportsMap <- gets exportsMap
    let passedExports = filter isModExport (S.toList (fromMaybe S.empty (M.lookup importPath exportsMap)))
        passedImports = map modExportToImport passedExports
    
    recurseResult <- concat <$> traverse gatherAllParentImports passedImports
    
    return (passedImports ++ recurseResult)
    where
        modExportToImport (ExportMod _ namespace) = Import nodeId namespace -- Pass on nodeId so all created imports 'reference' this one
        modExportToImport _ = error "(?) unreachable modExportToImport case"
