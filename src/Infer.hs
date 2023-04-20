module Infer where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe
import Data.Text (Text, pack, unpack)

import AnalysisError
import Kind
import Name
import NodeId
import Substitution
import Syntax
import Type

type TypeEnv = M.Map Name TypeScheme

type Infer = ExceptT AnalysisError (State InferState)
data InferState = InferState
    { curSubst :: Subst
    , count :: Int
    , env :: TypeEnv
    , typeConstrEnv :: M.Map (Name, Text) TypeScheme
    , declVars :: M.Map Text [TVar]
    } deriving (Show)

inferProgram :: BaseProgram -> Either AnalysisError TypedProgram
inferProgram modules =
    case runState (runExceptT runInferProgram) initInferState of
        (Left err, _) -> Left err
        (Right prog, st) -> Right (map (fmap (apply (curSubst st))) prog)
    where
        runInferProgram = traverse inferModule modules
        initInferState =
            InferState
            { curSubst = nullSubst
            , count = 0
            , env = M.empty
            , typeConstrEnv = M.empty
            , declVars = M.empty
            }

inferModule :: BaseModule -> Infer TypedModule
inferModule m = do
    s <- get
    put (s { declVars = M.empty })
    
    let mDecls = decls m

    mapM_ prepareDecl mDecls

    inferredDecls <- traverse inferDecl mDecls

    return (m { decls = inferredDecls })

-- Each declaration has a 'prepare' phase and also the actual 'infer' phase.
-- Preparation preceeds any inference in the module. It is useful for creating
-- type constructors for example.

prepareDecl :: BaseDecl -> Infer ()
prepareDecl (DData nodeId typeName typeParams constrs) = mapM_ insertTypeConstr constrs
    where
        dataType =
            let typeParams' = map TVar typeParams
                conType = \k -> TCon nodeId (TC typeName k)
            in case typeParams' of
                [] -> conType KStar
                _ ->
                    let k = foldl1 KArrow (map (const KStar) typeParams')
                    in foldl1 (.) (flip TApp <$> reverse typeParams') (conType k)

        insertTypeConstr (TypeConstr constrNodeId constrLabel constrTypes) = do
            let typeParamsSet = S.fromList typeParams
                freeTypeVars = ftv constrTypes
            
            when ((typeParamsSet `S.intersection` freeTypeVars) /= freeTypeVars) $ do
                -- It would be better to show all undefined variables in one error message,
                -- instead of just showing the first one. Maybe create a new AnalysisError variant?
                let undefinedVars = S.toList (freeTypeVars `S.difference` typeParamsSet)
                    (TV undefinedVar _) = head undefinedVars
                throwError (UndefinedIdentifier (unpack undefinedVar) constrNodeId [])

            let constrType =
                    case constrTypes of
                        [] -> dataType
                        _ -> foldr (TApp . TApp (tArrow constrNodeId)) dataType constrTypes
                constrTypeScheme = Forall typeParams constrType
            
            s <- get
            put (s { typeConstrEnv = M.insert (typeName, constrLabel) constrTypeScheme (typeConstrEnv s) })

prepareDecl (DLetDecl _ name _ _) = do
    s <- get
    put (s { declVars = M.insert (getIdentifier name) [] (declVars s) })

inferDecl :: BaseDecl -> Infer TypedDecl
inferDecl (DData nodeId typeName typeParams constrs) = return (DData nodeId typeName typeParams constrs)
inferDecl (DLetDecl nodeId name@(Name _ varName) typeAnn body) = do
    inferredBody <- inferExpr body
    
    subst1 <- gets curSubst
    let bodyType = apply subst1 (typeOfExpr inferredBody)

    dvs <- gets declVars
    let tmpTypeVars =
            case M.lookup varName dvs of
                Just vs -> vs
                Nothing -> error "(?) inferDecl DLetDecl unreachable case"
    mapM_ (constrain bodyType) (map TVar tmpTypeVars)
    
    s <- get
    put (s { declVars = M.delete varName (declVars s) })
    
    case typeAnn of
        Nothing -> return ()
        Just ann -> constrain bodyType ann

    subst2 <- gets curSubst
        
    env <- gets env
    addToTypeEnv name (generalize env (apply subst2 bodyType))

    return (DLetDecl nodeId name typeAnn inferredBody)   

inferExpr :: BaseExpr -> Infer TypedExpr
inferExpr = \case
    BaseELit nodeId lit ->
        let typ = inferLit nodeId lit
        in return (ELit nodeId typ lit)

    BaseEVar nodeId name -> do
        typ <- lookupType name
        return (EVar nodeId typ name)
    
    BaseEApp nodeId a b -> do
        retType <- TVar <$> freshVar

        inferredA <- inferExpr a
        inferredB <- inferExpr b
        
        let aType = typeOfExpr inferredA
            bType = typeOfExpr inferredB
        
        let toConstrain = TApp (TApp (tArrow nodeId) bType) retType
        constrain aType toConstrain

        return (EApp nodeId retType inferredA inferredB)
    
    BaseELambda nodeId paramName body -> do
        paramType <- TVar <$> freshVar

        inferredBody <- scoped paramName (Forall [] paramType) (inferExpr body)
        let bodyType = typeOfExpr inferredBody

        let lambdaType = TApp (TApp (tArrow nodeId) paramType) bodyType
        return (ELambda nodeId lambdaType paramName inferredBody)
    
    BaseETypeAnn nodeId expr ann -> do
        inferredExpr <- inferExpr expr
        let exprType = typeOfExpr inferredExpr
        
        constrain exprType ann
        return (ETypeAnn nodeId exprType inferredExpr ann)
    
    BaseELetExpr nodeId varName expr body -> do
        inferredExpr <- inferExpr expr
        let exprType = typeOfExpr inferredExpr
        
        inferredBody <- scoped varName (Forall [] exprType) (inferExpr body)
        let bodyType = typeOfExpr inferredBody

        return (ELetExpr nodeId bodyType varName inferredExpr inferredBody)
    
    BaseEIfExpr nodeId c a b -> do
        c' <- inferExpr c
        a' <- inferExpr a
        b' <- inferExpr b

        let ct = typeOfExpr c'
            at = typeOfExpr a'
            bt = typeOfExpr b'

        constrain ct (tBool nodeId)
        constrain at bt

        return (EIfExpr nodeId at c' a' b')
    
    BaseEMatch nodeId expr branches -> do
        inferredExpr <- inferExpr expr
        let exprType = typeOfExpr inferredExpr

        (exprConstraints, pats, branchExprs) <- unzip3 <$> traverse inferBranch branches

        mapM_ (constrain exprType) (concat exprConstraints)

        let branchExprTypes = map typeOfExpr branchExprs
        branchType <- TVar <$> freshVar
        mapM_ (constrain branchType) branchExprTypes

        return (EMatch nodeId branchType inferredExpr (zip pats branchExprs))
    
    where
        inferBranch (PWild, expr) = ([], PWild, ) <$> inferExpr expr
        inferBranch (PVar name, expr) = do
            mexprType <- TVar <$> freshVar
            inferredExpr <- scoped name (Forall [] mexprType) (inferExpr expr)
            return ([mexprType], PVar name, inferredExpr) 
        inferBranch (PVariant nodeId typeName variantLabel varNames, expr) = do
            varTypes <- traverse (const (TVar <$> freshVar)) varNames
            
            variantConstrType <- lookupTypeConstr typeName variantLabel
            mexprType <- TVar <$> freshVar
            
            let toConstrain =
                    case varTypes of
                        [] -> mexprType
                        _ -> foldr (TApp . TApp (tArrow nodeId)) mexprType varTypes
            
            constrain variantConstrType toConstrain

            let envAddition = M.fromList (zip varNames (map (Forall []) varTypes))

            s <- get
            put (s { env = (env s) `M.union` envAddition })

            inferredExpr <- inferExpr expr
            
            put s

            return ([mexprType], PVariant nodeId typeName variantLabel varNames, inferredExpr)
        inferBranch (PLit nodeId lit, expr) = ([inferLit nodeId lit], PLit nodeId lit, ) <$> inferExpr expr

inferLit :: NodeId -> Lit -> Type
inferLit nodeId = \case
    LInt {} -> tInt32 nodeId
    LFloat {} -> tFloat32 nodeId
    LChar {} -> tChar nodeId
    LString {} -> tString nodeId
    LBool {} -> tBool nodeId
    LUnit {} -> tUnit nodeId

addToTypeEnv :: Name -> TypeScheme -> Infer ()
addToTypeEnv name scheme = do
    s <- get
    put (s { env = M.insert name scheme (env s) })

scoped :: Name -> TypeScheme -> Infer a -> Infer a
scoped name scheme f = do
    initEnv <- gets env
    addToTypeEnv name scheme
    res <- f
    s <- get
    put (s { env = initEnv })
    return res

lookupType :: Name -> Infer Type
lookupType name = do
    env <- gets env
    case M.lookup name env of
        Just t -> instantiate t
        Nothing -> do
            declVars <- gets declVars
            let ident = getIdentifier name
            case M.lookup ident declVars of
                Nothing -> error ("(?) unreachable, undefined variable " ++ show name)
                Just tvars -> do
                    newVar <- freshVar
                    s <- get
                    put (s { declVars = M.insert ident (newVar : tvars) declVars })
                    return (TVar newVar)

lookupTypeConstr :: Name -> Text -> Infer Type
lookupTypeConstr typeName label = do
    constrEnv <- gets typeConstrEnv
    let typ = fromJust (M.lookup (typeName, label) constrEnv)
    instantiate typ

freshVar :: Infer TVar
freshVar = do
    s <- get
    put (s { count = count s + 1})
    return . flip TV KStar . pack . ('_':) $ ([1..] >>= flip replicateM ['a'..'z']) !! count s

generalize :: TypeEnv -> Type -> TypeScheme
generalize env t = Forall (S.toList vs) t
    where
        vs = ftv t `S.difference` ftv (M.elems env)

instantiate :: TypeScheme -> Infer Type
instantiate (Forall vs t) = do
    freshVars <- traverse ((TVar <$>) . const freshVar) vs
    let subst = M.fromList (zip vs freshVars)
    return (apply subst t)

constrain :: Type -> Type -> Infer ()
constrain a b = do
    s <- gets curSubst
    u <- unify (apply s a) (apply s b)
    st <- get
    put (st { curSubst = compose s u })

unify :: MonadError AnalysisError m => Type -> Type -> m Subst
unify (TApp a b) (TApp a' b') = do
    s1 <- unify a a'
    s2 <- unify (apply s1 b) (apply s1 b')
    return (s2 `compose` s1)
unify (TVar u) t = unifyVar u t
unify t (TVar u) = unifyVar u t
unify t1@(TCon nodeId1 a) t2@(TCon nodeId2 b)
    | a == b = return mempty
    | otherwise = throwError (TypeMismatch t1 t2 nodeId1 (Just nodeId2))
unify t1@(TApp _ _) t2@(TCon nodeId _)
    = throwError (TypeMismatch t1 t2 nodeId Nothing)
unify t1@(TCon nodeId _) t2@(TApp _ _)
    = throwError (TypeMismatch t1 t2 nodeId Nothing)

unifyVar :: MonadError AnalysisError m => TVar -> Type -> m Subst
unifyVar u t
    | t == TVar u = return mempty
    | u `S.member` ftv t = throwError (UndefinedIdentifier "" 1 [])
    | otherwise = return (M.fromList [(u, t)])
