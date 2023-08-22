module Infer where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Set as S

import Data.List
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
    , traitDefEnv :: M.Map (Name, Text) TypeScheme
    , traitConstraints :: M.Map TVar [Name]
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
            , traitDefEnv = M.empty
            , traitConstraints = M.empty
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
prepareDecl (DData nodeId typeName typeParamNames constrs) = mapM_ insertTypeConstr constrs
    where
        collectConstrFtvs (TypeConstr _ _ constrTypes) = ftv constrTypes

        getTypeParams = do
            -- 1. Collect all the free type variables from the type constructors
            -- 2. Ensure all are specified in typeParamNames
            -- 3. Check if their kinds match
            
            let constrFtvs = concatMap (S.toList . collectConstrFtvs) constrs

            -- Since we just want to compare type variable names first, we can use getTVText and ignore the kinds
            let freeTypeVarNames = nub (map getTVText constrFtvs)
            when ((typeParamNames `intersect` freeTypeVarNames) /= freeTypeVarNames) $ do
                -- It would be better to show all undefined variables in one error message,
                -- instead of just showing the first one. Maybe create a new AnalysisError variant?
                let undefinedVars = freeTypeVarNames \\ typeParamNames
                throwError (NotInScope (unpack $ head undefinedVars) nodeId [])
            
            let ftvKindPairs = nub [(name, k) | TV name k <- constrFtvs]
                checkKindMismatch [] = return ()
                checkKindMismatch ((name, k) : rest) = do
                    case lookup name rest of
                        Nothing -> checkKindMismatch rest
                        Just k' -> throwError (KindMismatch k k' nodeId)

            checkKindMismatch ftvKindPairs
            
            let finalTypeParams =
                    [TV name k | name <- typeParamNames, let k = fromMaybe KStar (lookup name ftvKindPairs)]

            return finalTypeParams

        getDataType = do
            typeParams <- map TVar <$> getTypeParams
            let conType = \k -> TCon nodeId (TC typeName k)
            case typeParams of
                [] -> return (conType KStar)
                _ ->
                    let k = KArrow KStar (foldl1 KArrow (map (const KStar) typeParams))
                    in return (foldl1 (.) (flip TApp <$> reverse typeParams) (conType k))

        insertTypeConstr (TypeConstr constrNodeId constrLabel constrTypes) = do
            typeParams <- getTypeParams
            dataType <- getDataType

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
    
prepareDecl (DTraitDecl nodeId traitName typeParamName defs) = do
    -- 1. Collect all free type variables from the definitions
    -- 2. Check if the kinds match for the type parameter
    -- 3. Insert relevant things into the trait map, also keep track of the kind for the type parameter
    
    let collectDefFtvs = \(TraitDef _ _ t) -> ftv t
        defFtvs = concatMap (S.toList . collectDefFtvs) defs
        ftvKindPairsInit = [(name, k) | TV name k <- defFtvs]
        refKind =
            case lookup typeParamName ftvKindPairsInit of
                Nothing -> error "type param never used in trait decl"
                Just k -> k
        ftvKindPairs = (nub ftvKindPairsInit) \\ [(typeParamName, refKind)]
    
    -- If there's any type variable left with the typeParamName then the kinds don't match
    case lookup typeParamName ftvKindPairs of
        Nothing -> return ()
        Just k' -> throwError (KindMismatch refKind k' nodeId)
    

    undefined
prepareDecl (DImplDecl {}) = undefined

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
    mapM_ (constrain nodeId bodyType) (map TVar tmpTypeVars)
    
    s <- get
    put (s { declVars = M.delete varName (declVars s) })
    
    case typeAnn of
        Nothing -> return ()
        Just ann -> do
            -- Replace all type variables with fresh ones to prevent type variable name collisions
            freshAnn <- instantiate (generalize M.empty ann)
            constrain nodeId bodyType freshAnn

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
        typ <- lookupType nodeId name
        return (EVar nodeId typ name)
    
    BaseEApp nodeId a b -> do
        retType <- TVar <$> freshVar KStar

        inferredA <- inferExpr a
        inferredB <- inferExpr b
        
        let aType = typeOfExpr inferredA
            bType = typeOfExpr inferredB

        let toConstrain = TApp (TApp (tArrow nodeId) bType) retType
        constrain nodeId toConstrain aType

        return (EApp nodeId retType inferredA inferredB)
    
    BaseELambda nodeId paramName body -> do
        paramType <- TVar <$> freshVar KStar

        inferredBody <- scoped paramName (Forall [] paramType) (inferExpr body)
        let bodyType = typeOfExpr inferredBody

        let lambdaType = TApp (TApp (tArrow nodeId) paramType) bodyType
        return (ELambda nodeId lambdaType paramName inferredBody)
    
    BaseETypeAnn nodeId expr ann -> do
        inferredExpr <- inferExpr expr
        let exprType = typeOfExpr inferredExpr
        
        constrain nodeId exprType ann
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
        
        let cId = nodeIdOfExpr c'
            bId = nodeIdOfExpr b'

        constrain cId (tBool cId) ct
        constrain bId at bt -- bId so error report happens for else case

        return (EIfExpr nodeId at c' a' b')
    
    BaseEMatch nodeId expr branches -> do
        inferredExpr <- inferExpr expr
        let exprType = typeOfExpr inferredExpr
            exprNodeId = nodeIdOfExpr inferredExpr

        (exprConstraints, pats, branchExprs) <- unzip3 <$> traverse inferBranch branches

        mapM_ (constrain exprNodeId exprType) (concat exprConstraints)

        branchType <- TVar <$> freshVar KStar
        sequence_ [constrain (nodeIdOfExpr branchExpr) branchType (typeOfExpr branchExpr) | branchExpr <- branchExprs]

        return (EMatch nodeId branchType inferredExpr (zip pats branchExprs))

    BaseEDoubleColon nodeId typeName constrLabel -> do
        constrType <- lookupTypeConstr nodeId typeName constrLabel
        return (EDoubleColon nodeId constrType typeName constrLabel)
    
    BaseERecordEmpty nodeId -> return (ERecordEmpty nodeId TRecordEmpty)
    
    BaseERecordExtend nodeId record label expr -> do
        inferredRecord <- inferExpr record
        inferredExpr <- inferExpr expr
        let recordType = typeOfExpr inferredRecord
            exprType = typeOfExpr inferredExpr
        
        let resType = TRecordExtend label exprType recordType
        return (ERecordExtend nodeId resType inferredRecord label inferredExpr)

    where
        inferBranch (PWild nodeId, expr) = ([], PWild nodeId, ) <$> inferExpr expr
        inferBranch (PVar nodeId name, expr) = do
            mexprType <- TVar <$> freshVar KStar
            inferredExpr <- scoped name (Forall [] mexprType) (inferExpr expr)
            return ([mexprType], PVar nodeId name, inferredExpr) 
        inferBranch (PVariant nodeId typeName variantLabel varNames, expr) = do
            varTypes <- traverse (const (TVar <$> freshVar KStar)) varNames
            
            variantConstrType <- lookupTypeConstr nodeId typeName variantLabel
            mexprType <- TVar <$> freshVar KStar
            
            let toConstrain =
                    case varTypes of
                        [] -> mexprType
                        _ -> foldr (TApp . TApp (tArrow nodeId)) mexprType varTypes
            
            constrain nodeId variantConstrType toConstrain

            let envAddition = M.fromList (zip varNames (map (Forall []) varTypes))
            inferredExpr <- scopedModify (`M.union` envAddition) (inferExpr expr)

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

scopedModify :: (TypeEnv -> TypeEnv) -> Infer a -> Infer a
scopedModify envf f = do
    initEnv <- gets env
    
    s <- get
    put (s { env = envf initEnv })

    res <- f

    s' <- get
    put (s' { env = initEnv })

    return res

lookupType :: NodeId -> Name -> Infer Type
lookupType nodeId name = do
    env <- gets env
    case M.lookup name env of
        Just t -> instantiate t
        Nothing -> do
            declVars <- gets declVars
            let ident = getIdentifier name
            case M.lookup ident declVars of
                Nothing ->
                    -- So the only possibility here should be that the identifier was defined
                    -- in the name resolution pass, but is actually the NAME of a TYPE.
                    -- Hence why it is not in the type environment or in declVars.
                    -- Maybe add a different error variant for this?
                    let identString = unpack ident
                    in throwError (NotInScope identString nodeId ['\'' : identString ++ "' is a type, not a variable"])

                Just tvars -> do
                    newVar <- freshVar KStar -- All variable types should have kind KStar
                    s <- get
                    put (s { declVars = M.insert ident (newVar : tvars) declVars })
                    return (TVar newVar)

lookupTypeConstr :: NodeId -> Name -> Text -> Infer Type
lookupTypeConstr nodeId typeName label = do
    constrEnv <- gets typeConstrEnv
    case M.lookup (typeName, label) constrEnv of
        Nothing -> do
            -- The name resolution pass guarantees that the type is in scope, so the constructor must not be
            let constrIdent = unpack label
                typeIdent = unpack (getIdentifier typeName)
                hint = "The type constructor '" ++ constrIdent ++ "' doesn't exist"
                
            throwError (NotInScope (typeIdent ++ "::" ++ constrIdent) nodeId [hint])

        Just typ -> instantiate typ

freshVar :: Kind -> Infer TVar
freshVar k = do
    s <- get
    put (s { count = count s + 1})
    return . flip TV k . pack . ('$':) $ ([1..] >>= flip replicateM ['a'..'z']) !! count s

generalize :: TypeEnv -> Type -> TypeScheme
generalize env t = Forall (S.toList vs) t
    where
        vs = ftv t `S.difference` ftv (M.elems env)

instantiate :: TypeScheme -> Infer Type
instantiate (Forall vs t) = do
    freshVars <- sequence [(v, ) . TVar <$> freshVar k | v@(TV _ k) <- vs]
    let subst = M.fromList freshVars
    return (apply subst t)

constrain :: NodeId -> Type -> Type -> Infer ()
constrain nodeId a b = do
    s <- gets curSubst
    u <- unify nodeId (apply s a) (apply s b)
    st <- get
    put (st { curSubst = compose s u })

unify :: NodeId -> Type -> Type -> Infer Subst
unify nodeId (TApp a b) (TApp a' b') = do
    s1 <- unify nodeId a a'
    s2 <- unify nodeId (apply s1 b) (apply s1 b')
    return (s2 `compose` s1)

unify nodeId (TVar u) t = unifyVar nodeId u t

unify nodeId t (TVar u) = unifyVar nodeId u t

unify nodeId t1@(TCon _ a) t2@(TCon _ b)
    | a == b = return nullSubst
    | otherwise = throwError (TypeMismatch t1 t2 nodeId)

unify nodeId (TRecordExtend label1 typ1 rest1) t2@TRecordExtend {} = do
    (rest2, s1) <- rewriteRow nodeId t2 t2 label1 typ1
    s2 <- unify nodeId (apply s1 rest1) (apply s1 rest2)
    return (s2 `compose` s1)

unify _ TRecordEmpty TRecordEmpty = return nullSubst

unify nodeId t1 t2 = throwError (TypeMismatch t1 t2 nodeId)

unifyVar :: NodeId -> TVar -> Type -> Infer Subst
unifyVar nodeId u t
    | t == TVar u = return mempty
    | u `S.member` ftv t = throwError (OccursCheckFail u t nodeId)
    | kind u /= kind t = throwError (KindMismatch (kind u) (kind t) nodeId)
    | otherwise = return (M.singleton u t)

rewriteRow :: NodeId -> Type -> Type -> Text -> Type -> Infer (Type, Subst)
rewriteRow nodeId originalRow2 row2 label1 typ1 =
    case row2 of
        TRecordEmpty -> throwError (RecordTypeMissingField originalRow2 label1 nodeId) -- Row doesn't contain label error
        TRecordExtend label2 typ2 rest2 | label2 == label1 -> do
            s <- unify nodeId typ1 typ2
            return (apply s rest2, s)
        TRecordExtend label2 typ2 rest2 -> do
            (recurseTyp, recurseSub) <- rewriteRow nodeId originalRow2 rest2 label1 typ1
            return (apply recurseSub (TRecordExtend label2 typ2 recurseTyp), recurseSub)
        tv@TVar {} -> do
            restTv <- TVar <$> freshVar KStar
            s <- unify nodeId tv (TRecordExtend label1 typ1 restTv)
            return (apply s restTv, s)
        other -> throwError (ExpectedRecordType other nodeId) -- Expected row type for row2
