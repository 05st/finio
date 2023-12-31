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
import DeclarationSort
import Kind
import Name
import NodeId
import Substitution
import Syntax
import Type

import Debug.Trace

type TypeEnv = M.Map Name TypeScheme

type Infer = ExceptT AnalysisError (State InferState)
data InferState = InferState
    { curSubst :: Subst
    , count :: Int
    , env :: TypeEnv
    , typeConstrEnv :: M.Map (Name, Text) TypeScheme
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
            }

inferModule :: BaseModule -> Infer TypedModule
inferModule m = do
    -- Perform dependency analysis on declarations
    let mDecls = decls m
        letDecls = filter isLetDecl mDecls
    
    let letDeclGroups = sortDeclarations letDecls
    trace (show letDeclGroups) $ return ()

    mapM_ prepareDecl mDecls

    inferredDecls <- traverse inferDecl mDecls

    return (m { decls = inferredDecls })

prepareDecl :: BaseDecl -> Infer ()
prepareDecl = undefined

inferDecl :: BaseDecl -> Infer TypedDecl
inferDecl = undefined
