module Resolver where

import Data.Text (Text, pack, unpack)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Arrow

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import AnalysisError
import Syntax
import Name
import Type
import NodeId

type Resolve = ExceptT AnalysisError (ReaderT Namespace (State ResolverState))
data ResolverState = ResolverState
    { exportsMap :: M.Map Namespace (S.Set Export)
    , nameSet :: S.Set Name
    , modImports :: [Import]
    , modNamespace :: Namespace
    , tempScopeCount :: Int
    } deriving (Show)

-- Utility functions
tempVar :: Resolve Text
tempVar = do
    s <- get
    put (s { tempScopeCount = tempScopeCount s + 1 })
    return . pack . ('_':) $ ([1..] >>= flip replicateM ['a'..'z']) !! tempScopeCount s

tempScope :: Resolve Namespace
tempScope = do
    curScope <- ask
    newScope <- tempVar
    return (curScope ++ [newScope])

addName :: Name -> Resolve ()
addName name = modify (\s -> s { nameSet = S.insert name (nameSet s) })

resolveProgram :: BaseProgram -> Either AnalysisError BaseProgram
resolveProgram modules = evalState (runReaderT (runExceptT (traverse resolveModule modules)) []) initResolverState
    where
        initResolverState =
            ResolverState { exportsMap = initExportsMap, nameSet = S.empty, modImports = [], modNamespace = [], tempScopeCount = 0}
        initExportsMap =
            M.fromList (map (modPath &&& (S.fromList . exports)) modules)

resolveModule :: BaseModule -> Resolve BaseModule
resolveModule m = do
    let allExports = exports m
    
    let declExports = filter isDeclExport allExports
        moduleExports = filter isModExport allExports
        typeExports = filter isTypeExport allExports
        
    let moduleImports = map importPath (imports m)
    
    let declNames = map (getIdentifier . getDeclName) (decls m) -- All decl names should be unqualified right now

    -- Verify all exported modules were also imported
    let notImported = filter ((`notElem` moduleImports) . exportedModName) moduleExports
    unless (null notImported) (throwError (ExportedModulesNotImported notImported))

    -- Verify all exported declarations are defined
    let notDefined = filter ((`notElem` declNames) . exportedDeclName) declExports
    unless (null notDefined) (throwError (ExportedDeclsNotDefined notDefined))

    let namespace = modPath m
        initNameSet = S.fromList (map (\n -> Name namespace n) declNames)

    s <- get
    put (s { nameSet = initNameSet, modImports = imports m, modNamespace = namespace })

    resolvedDecls <- local (++ namespace) (traverse resolveDecl (decls m))
    
    put s

    return (m { decls = resolvedDecls })
    
    where
        getDeclName (DLetDecl _ name _ _) = name
        getDeclName _ = undefined

resolveDecl :: BaseDecl -> Resolve BaseDecl
resolveDecl (DLetDecl nodeId varName typeAnn expr) = do
    DLetDecl nodeId varName <$> resolveTypeAnn typeAnn <*> resolveExpr expr
resolveDecl _ = undefined

resolveExpr :: BaseExpr -> Resolve BaseExpr
resolveExpr (BaseELit nodeId lit) =
    return (BaseELit nodeId lit)

resolveExpr (BaseEVar nodeId varName) = do
    case varName of
        Name [] i -> do
            namespace <- resolveName nodeId i
            return (BaseEVar nodeId (Name namespace i))
        Name ns i -> do
            -- TODO: Verify that the qualified name exists
            undefined

resolveExpr (BaseEApp nodeId a b) =
    BaseEApp nodeId <$> resolveExpr a <*> resolveExpr b

resolveExpr (BaseELambda nodeId (Name _ i) expr) = do
    newScope <- tempScope
    let paramName = Name newScope i
    
    addName paramName
    
    resolvedExpr <- local (const newScope) (resolveExpr expr)

    return (BaseELambda nodeId paramName resolvedExpr)

resolveExpr (BaseETypeAnn nodeId expr typ) = do
    resolvedExpr <- resolveExpr expr
    resolvedType <- resolveType typ
    return (BaseETypeAnn nodeId resolvedExpr resolvedType)

resolveExpr (BaseELetExpr nodeId (Name _ i) expr body) = do
    resolvedExpr <- resolveExpr expr

    newScope <- tempScope
    let varName = Name newScope i
    
    addName varName

    resolvedBody <- local (const newScope) (resolveExpr body)
    
    return (BaseELetExpr nodeId varName resolvedExpr resolvedBody)

resolveExpr (BaseEIfExpr nodeId c a b) =
    BaseEIfExpr nodeId <$> resolveExpr c <*> resolveExpr a <*> resolveExpr b

resolveExpr (BaseEMatch nodeId expr branches) = do
    BaseEMatch nodeId <$> resolveExpr expr <*> traverse resolveBranch branches
    where
        resolveBranch (p@PLit {}, e) = (p, ) <$> resolveExpr e
        resolveBranch (PWild, e) = (PWild, ) <$> resolveExpr e
        resolveBranch (PVar (Name _ i), e) = do
            newScope <- tempScope
            let varName = Name newScope i
            addName varName

            resolvedExpr <- local (const newScope) (resolveExpr e)

            return (PVar varName, resolvedExpr)

resolveTypeAnn :: Maybe Type -> Resolve (Maybe Type)
resolveTypeAnn = traverse resolveType

resolveType :: Type -> Resolve Type
resolveType t@(TCon nodeId (TC (Name ns i) k))
    | i `elem` primTypes && null ns = return t
    | otherwise = do
        case ns of
            [] -> do
                namespace <- resolveName nodeId i
                -- TODO: VERIFY namespace::typeName IS ACTUALLY A TYPE
                return (TCon nodeId (TC (Name namespace i) k))
            _ -> do
                -- TODO: Verify that the qualified name exists
                undefined

resolveType (TApp a b) = TApp <$> resolveType a <*> resolveType b
resolveType t@TVar {} = return t

-- Finds the namespace
resolveName :: NodeId -> Text -> Resolve Namespace
resolveName varNodeId n = do
    modNamespace <- gets modNamespace
    
    -- Check module decls first
    namesSet <- gets nameSet
    if (Name modNamespace n) `S.member` namesSet
        then return modNamespace
        else ask >>= resolveRecursive n
    
    where
        resolveRecursive name curNamespace = do
            nameSet <- gets nameSet
            modNamespace <- gets modNamespace
            
            let qualifiedName = Name curNamespace name
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
                            [] -> throwError (UndefinedIdentifier (unpack name) varNodeId) -- Undefined

                            [Import _ namespace] -> return namespace

                            many -> throwError (MultipleDefinitions (unpack name) varNodeId many) -- Multiple definitions
                            -- Since nodeId is passed in modExportToImport, it is guaranteed that the nodeIds contain the same source in all Imports of 'many'
                            
        checkExport name imp@(Import _ namespace) = do
            exportsMap <- gets exportsMap
            let exports =
                    case M.lookup namespace exportsMap of
                        Nothing ->
                            error ("(?) Resolver.hs checkExport Nothing case: " ++ show namespace ++ '\n' : show exportsMap)
                        Just a -> a
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
