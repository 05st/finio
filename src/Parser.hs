{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import Data.Text (Text, pack)
import Data.List
import Data.Function
import qualified Data.IntMap as IM

import Text.Megaparsec hiding (ParseError, State)
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad
import Control.Monad.Combinators.Expr
import Control.Monad.Reader
import Control.Monad.State

import Error.Diagnose

import Lexer
import Syntax
import Type
import Kind
import NodeId
import Name

import Debug.Trace

-- Parses a single file/module
parse :: [OperatorDef] -> (FilePath, [FilePath], Text) -> State ParserState (Either ParseError BaseModule)
parse operatorDefs (filePath, modPath, input) =
    runParserT (runReaderT (parseModule (map pack modPath)) operatorDefs) filePath input

testParse :: Text -> Parser a -> Either ParseError a
testParse input f = evalState (runParserT (runReaderT f []) "parseSingle" input) (ParserState 0 mempty)

-- Utility functions

freshNodeId :: Parser NodeId
freshNodeId = do
    s <- get
    let cur = curNodeId s
    put (s { curNodeId = cur + 1 })
    return cur

withNodeId :: (NodeId -> Parser a) -> Parser a
withNodeId f = do
    nodeId <- freshNodeId
    start <- getSourcePos
    res <- f nodeId
    end <- getSourcePos
    let newPos = Position (unPos (sourceLine start), unPos (sourceColumn start)) (unPos (sourceLine end), unPos (sourceColumn end)) (sourceName start)
    modify (\s -> s { posMap = IM.insert nodeId newPos (posMap s) })
    return res

withNodeIdEndline :: (NodeId -> Parser a) -> Parser a
withNodeIdEndline f = (withNodeId f) <* endline

-- Parsing

parseModuleOperDefs :: Parser [OperatorDef]
parseModuleOperDefs =
    concat <$> manyTill p eof
    where
        p = (try ((:[]) <$> parseOperDef) <|> ([] <$ anySingle))

parseOperDef :: Parser OperatorDef
parseOperDef = do
    assoc <- parseAssoc
    prec <- decimal
    OperatorDef assoc prec <$> operator
    where
        parseAssoc
            =   (ALeft <$ symbol "infixl")
            <|> (ARight <$ symbol "infixr")
            <|> (ANone <$ symbol "infix")
            <|> (APrefix <$ symbol "prefix")
            <|> (APostfix <$ symbol "postfix")

parseModule :: [Text] -> Parser BaseModule
parseModule modPath = do
    imports <- many (try (parseImport <* endline))
    exports <- option [] (symbol "export" *> sepBy1 parseExportItem comma <* endline)
    
    many (parseOperDef <* endline) -- Ignore operator defs for main parsing pass

    parsedDecls <- manyTill parseDecl eof
    
    return (Module modPath imports exports parsedDecls)
    where
        parseImport = withNodeId $ \nodeId -> do
            symbol "import"
            Import nodeId <$> parseQualifiedName

        parseExportItem = parseModExport <|> parseDeclExport
        parseDeclExport = withNodeId $ \nodeId -> ExportDecl nodeId <$> identifier
        parseModExport = withNodeId $ \nodeId -> do
            symbol "module"
            ExportMod nodeId <$> parseQualifiedName

parseDecl :: Parser BaseDecl
parseDecl = parseLetDecl <|> (parseFnDecl >>= desugarFnDecl) <|> parseDataDecl

-- Desugars to a let decl + lambda (TODO: patterns)
parseFnDecl :: Parser FnDecl
parseFnDecl = (try parseFnWithTypeAnn <|> try parseFnIndented <|> parseFnBasic) <* endline
    where
        parseFnName = symbol "fn" *> (parens operator <|> identifier)
        parseFnBranch = (,) <$> some parsePattern <*> (symbol "=" *> parseExpr)

        -- Indented function with type annotation
        parseFnWithTypeAnn = indentBlock . withNodeId $ \nodeId -> do
            fnName <- parseFnName
            typeAnn <- Just <$> parseTypeAnn
            return (L.IndentSome Nothing (return . FnDecl nodeId fnName typeAnn) parseFnBranch)

        -- Indented function without type annotation
        parseFnIndented = indentBlock . withNodeId $ \nodeId -> do
            fnName <- parseFnName
            return (L.IndentSome Nothing (return . FnDecl nodeId fnName Nothing) parseFnBranch)

        -- No indentation (can't have type annotation, and only one branch)
        parseFnBasic = withNodeId $ \nodeId -> do
            fnName <- parseFnName
            FnDecl nodeId fnName Nothing <$> ((:[]) <$> parseFnBranch)


-- DESUGARING FN DECLARATIONS TO LET + LAMBDA + NESTED MATCH EXPRESSIONS

data FnBranch
    = BrNested Pattern [FnBranch] | BrExpr Pattern BaseExpr
    deriving (Show)

-- Used in figuring out the arity of the function desugared from fn decls
branchDepth :: FnBranch -> Int
branchDepth (BrExpr _ _) = 1
branchDepth (BrNested _ (child : _)) = 1 + branchDepth child
branchDepth _ = 0

-- Helper functions
extractPattern :: FnBranch -> Pattern
extractPattern (BrExpr p _) = p
extractPattern (BrNested p _) = p

extractChildren :: FnBranch -> [FnBranch]
extractChildren (BrExpr _ _) = []
extractChildren (BrNested _ cs) = cs

checkBranch :: Pattern -> FnBranch -> Bool
checkBranch pat (BrExpr pat' _) = pat `patEqIgnoreNodeId` pat'
checkBranch pat (BrNested pat' _) = pat `patEqIgnoreNodeId` pat'

-- Compare patterns while ignoring nodeId, have to write this instead of deriving Eq
patEqIgnoreNodeId :: Pattern -> Pattern -> Bool
patEqIgnoreNodeId (PVariant _ a b c) (PVariant _ a' b' c') = a == a' && b == b' && c == c'
patEqIgnoreNodeId (PLit _ a) (PLit _ a') = a == a'
patEqIgnoreNodeId (PVar _ a) (PVar _ a' ) = a == a'
patEqIgnoreNodeId (PWild _) (PWild _) = True
patEqIgnoreNodeId _ _ = False

-- Turn flat list of patterns and function expr to FnBranch tree
desugarBranch :: ([Pattern], BaseExpr) -> FnBranch
desugarBranch (p : [], expr) = BrExpr p expr
desugarBranch (p : ps, expr) = BrNested p [desugarBranch (ps, expr)]
desugarBranch _ = error "(?) desugarBranch called with empty pattern list"

-- "Grouping" functions below
-- Basically takes all consecutive branches with the same pattern and unifies them under one branch
groupAll :: [FnBranch] -> [FnBranch]
groupAll = map groupChildren . groupBranches

-- Recursively groups all children of a branch
groupChildren :: FnBranch -> FnBranch
groupChildren (BrNested p cs) = BrNested p (map groupChildren (groupBranches cs))
groupChildren a@(BrExpr _ _) = a

-- Groups consecutive branches which match the same pattern
-- It takes the first pattern as reference, and puts all consecutive similar branches (sharing patterns)
-- as a child of the reference branch
groupBranches :: [FnBranch] -> [FnBranch]
groupBranches [] = []
groupBranches (a@(BrExpr _ _) : rest) = a : groupBranches rest
groupBranches ((BrNested refPat initCs) : rest) =
    let (samePatternBrs, rest') = span (checkBranch refPat) rest
        newChildren = concatMap extractChildren samePatternBrs
    in BrNested refPat (initCs ++ newChildren) : groupBranches rest'

-- Functions that construct the match expressions when desugaring fn decls below
constructMatch :: Text -> NodeId -> [FnBranch] -> State Int BaseExpr
constructMatch ident nodeId branches = do
    nextIdent <- nextIdentifier
    matchBranches <- traverse (constructMatch' nextIdent) branches
    return $ BaseEMatch nodeId (BaseEVar nodeId (unqualified ident)) matchBranches

constructMatch' :: Text -> FnBranch -> State Int (Pattern, BaseExpr)
constructMatch' _ (BrExpr pat expr) = return (pat, expr)
constructMatch' ident (BrNested pat children) = do
    mexpr <- constructMatch ident (nodeIdOfPat pat) children
    return (pat, mexpr)

freeIdents :: [Text]
freeIdents = map (pack . ('_':)) ([1..] >>= flip replicateM ['a'..'z']) 

nextIdentifier :: State Int Text
nextIdentifier = do
    n <- get
    put (n+1)
    return $ freeIdents !! n

-- Desugars fn decls to let + lambda + nested match expressions
desugarFnDecl :: FnDecl -> Parser BaseDecl
desugarFnDecl (FnDecl nodeId fnName typeAnn flatBranches) = do
    let treeBranches = map desugarBranch flatBranches
        depths@(arity : _) = map branchDepth treeBranches -- depths should never be an empty list
    
    if (all (== arity) depths) then do
        let groupedBranches = groupAll treeBranches
        trace (show groupedBranches) $ return ()
        
        let [initBranch] = groupedBranches -- there should only be one top level branch (for the first parameter)
            (fnexpr, _) = runState (constructMatch "_a" (nodeIdOfPat . extractPattern $ initBranch) [initBranch]) 1
        
        let params = take arity freeIdents

        let lambda = foldr (BaseELambda nodeId) fnexpr (map unqualified params)
        return $ DLetDecl nodeId (unqualified fnName) typeAnn lambda
    else
        fail "all branches should be same length in fn decl"
    
-- desugarFnDecl (FnDecl _ _ _ []) = error "(?) desugarFnDecl called with empty branch list"
-- desugarFnDecl _ = error "(!) fn declarations don't support more than one branch yet"


--

parseLetDecl :: Parser BaseDecl
parseLetDecl = withNodeIdEndline $ \nodeId -> do
    symbol "let"
    name <- (parens operator <|> identifier)
    typeAnn <- optional parseTypeAnn
    symbol "="
    DLetDecl nodeId (unqualified name) typeAnn <$> parseExpr

parseDataDecl :: Parser BaseDecl
parseDataDecl = withNodeIdEndline $ \nodeId -> do
    symbol "data"
    name <- typeIdentifier
    typeVarNames <- many identifier
    symbol "="
    constrs <- sepBy1 parseConstructor (symbol "|")
    return (DData nodeId (unqualified name) typeVarNames constrs)
    where
        parseConstructor = withNodeId $ \nodeId -> do
            constrName <- typeIdentifier
            constrTypes <- many parseBaseType
            return (TypeConstr nodeId constrName constrTypes)
    
parseExpr :: Parser BaseExpr
parseExpr = do
    opers <- ask

    let table = mkTable opers
    makeExprParser parseTerm table
    where
        mkTable = map (map toParser) . groupBy ((==) `on` prec) . sortBy (flip compare `on` prec)
        toParser (OperatorDef assoc _ oper) =
            let name = unqualified oper in
            case assoc of
                ANone -> infixOp oper (flip eBinOp name)
                ALeft -> infixlOp oper (flip eBinOp name)
                ARight -> infixrOp oper (flip eBinOp name)
                APrefix -> prefixOp oper (flip eUnaOp name)
                APostfix -> postfixOp oper (flip eUnaOp name)
        infixOp name f = InfixN (withNodeId $ \nodeId -> (f nodeId <$ symbol name))
        infixlOp name f = InfixL (withNodeId $ \nodeId -> (f nodeId <$ symbol name))
        infixrOp name f = InfixR (withNodeId $ \nodeId -> (f nodeId <$ symbol name))
        prefixOp name f = Prefix (withNodeId $ \nodeId -> (f nodeId <$ symbol name))
        postfixOp name f = Postfix (withNodeId $ \nodeId -> (f nodeId <$ symbol name))

parseTerm :: Parser BaseExpr
parseTerm = parseIfExpr <|> parseLetExpr <|> parseMatchExpr <|> parseFnApp

parseIfExpr :: Parser BaseExpr
parseIfExpr = withNodeId $ \nodeId -> do
    symbol "if"
    cond <- parseExpr
    symbol "then"
    onTrue <- parseExpr
    onFalse <- option (BaseELit nodeId LUnit) (symbol "else" *> parseExpr)
    return (BaseEIfExpr nodeId cond onTrue onFalse)

parseLetExpr :: Parser BaseExpr
parseLetExpr = withNodeId $ \nodeId -> do
    symbol "let"
    name <- identifier
    symbol "="
    expr <- parseExpr
    symbol "in"
    body <- parseExpr
    return (BaseELetExpr nodeId (unqualified name) expr body)

parseMatchExpr :: Parser BaseExpr
parseMatchExpr = indentBlock . withNodeId $ \nodeId -> do
    symbol "match"
    expr <- parseExpr
    return (L.IndentSome Nothing (return . BaseEMatch nodeId expr) parseBranch)
    where
        parseBranch = do
            pat <- parsePattern
            symbol "=>"
            (pat, ) <$> parseExpr

parseFnApp :: Parser BaseExpr
parseFnApp = withNodeId $ \nodeId -> do
    exprs <- some parseValue
    case exprs of
        [a] -> return a
        fn : args -> return (foldl1 (.) (flip (BaseEApp nodeId) <$> reverse args) fn)
        [] -> error "(?) parseFnApp unreachable case"

parseValue :: Parser BaseExpr
parseValue = parseLambda <|> parseLitExpr <|> parseRecord <|> try parseVariant <|> try parseVariable <|> parensExpr
    where
        parseLitExpr = withNodeId $ \nodeId -> BaseELit nodeId <$> parseLit
        parensExpr = parens (do
            expr <- parseExpr
            option expr (withNodeId $ \nodeId -> BaseETypeAnn nodeId expr <$> parseTypeAnn)) -- Type annotated expression

parseLambda :: Parser BaseExpr
parseLambda = withNodeId $ \nodeId -> do
    symbol "\\"
    params <- some identifier
    symbol "->"
    expr <- parseExpr
    
    return (foldr (BaseELambda nodeId) expr (map unqualified params))

parseVariant :: Parser BaseExpr
parseVariant = withNodeId $ \nodeId -> do
    typeName <- typeIdentifier
    symbol "::"
    constrLabel <- identifier
    return (BaseEVariant nodeId (unqualified typeName) constrLabel)

parseVariable :: Parser BaseExpr
parseVariable = parseRegular <|> parens parseOperator
    where
        parseRegular = withNodeId $ \nodeId -> do
            idents <- parseQualifiedName
            case reverse idents of
                [name] -> return (BaseEVar nodeId (unqualified name))
                (name : ns) -> return (BaseEVar nodeId (Name ns name))
                _ -> error "(?) parseVariable unreachable case"
        parseOperator =
            withNodeId $ \nodeId -> BaseEVar nodeId . unqualified <$> operator

parseLit :: Parser Lit
parseLit = try (LFloat <$> float) <|> (LInt <$> integer)
    <|> (LChar <$> charLiteral) <|> (LString . pack <$> stringLiteral)
    <|> (LBool True <$ symbol "true") <|> (LBool False <$ symbol "false")
    <|> (LUnit <$ symbol "()")
    where
        integer = decimal -- <|> try octal <|> try binary <|> hexadecimal

parseRecord :: Parser BaseExpr
parseRecord = withNodeId $ \nodeId -> braces $ do
    extends <- option (BaseERecordEmpty nodeId) (symbol ".." *> parseVariable <* comma)
    items <- sepBy recordItem comma

    let res = foldr (.) (const extends) [\r -> BaseERecordExtend nodeId r l e  | (l, e) <- items] ()
    return res
    where
        recordItem = (,) <$> identifier <*> (symbol "=" *> parseExpr)

parseTypeAnn :: Parser Type
parseTypeAnn = colon *> parseType

parseType :: Parser Type
parseType = try parseArrowType <|> parseTypeApp <?> "type"

parseArrowType :: Parser Type
parseArrowType = withNodeId $ \nodeId -> do
    paramType <- parseTypeApp
    symbol "->"
    TApp (TApp (tArrow nodeId) paramType) <$> parseType

parseTypeApp :: Parser Type
parseTypeApp = do
    types <- some parseBaseType
    case types of
        [a] -> return a
        (fnType : typeArgs) -> return (fixParsedTypeKind fnType typeArgs)
        [] -> error "(?) parseTypeApp unreachable case"

-- Fix the kinds (since all TCon/TVar kinds are parsed as KStar)
fixParsedTypeKind :: Type -> [Type] -> Type
fixParsedTypeKind appType typeArgs =
    let k = foldl1 KArrow (map (const KStar) (appType : typeArgs))
        fnType' =
            case appType of
                TCon nodeId (TC name _) -> TCon nodeId (TC name k)
                TVar (TV name _) -> TVar (TV name k)
                t@TApp {} -> t -- Parsed TApps should have the correct kind
                _ -> appType -- Other types should produce a kind error in the type inference pass
    in (foldl1 (.) (flip TApp <$> reverse typeArgs) fnType')

parseBaseType :: Parser Type
parseBaseType = parseRecordType <|> parseConcreteType <|> parseTypeVar <|> parens parseType

parseRecordType :: Parser Type
parseRecordType = braces (option TRecordEmpty rowExtend)
    where
        rowExtend = do
            rowsParsed <- sepBy1 row comma
            let rowExtends = map (uncurry TRecordExtend) rowsParsed
            extended <- option TRecordEmpty (symbol "|" *> parseRecordType) <?> "record type"

            pure (foldr ($) extended rowExtends)
        row = (,) <$> identifier <*> parseTypeAnn

parseConcreteType :: Parser Type
parseConcreteType = userDefined <|> parsePrimType
    where
        userDefined = withNodeId $
            \nodeId -> TCon nodeId . flip TC KStar . unqualified <$> typeIdentifier

parsePrimType :: Parser Type
parsePrimType = withNodeId $
    \nodeId -> TCon nodeId . flip TC KStar . unqualified <$> choice (map symbol (primTypes \\ ["->"])) -- "->"" should only be parsed in parseArrowType

parseTypeVar :: Parser Type
parseTypeVar = TVar . flip TV KStar <$> identifier

parsePattern :: Parser Pattern
parsePattern = parseWildPattern <|> parseVariantPattern <|> parseVarPattern <|> parseLitPattern <|> parens parsePattern

parseVariantPattern :: Parser Pattern
parseVariantPattern = withNodeId $ \nodeId -> do
    typeName <- typeIdentifier
    symbol "::"
    constrName <- typeIdentifier
    vars <- many identifier
    return (PVariant nodeId (unqualified typeName) constrName (map unqualified vars))

parseWildPattern :: Parser Pattern
parseWildPattern = withNodeId $ \nodeId -> PWild nodeId <$ symbol "_"

parseVarPattern :: Parser Pattern
parseVarPattern = withNodeId $ \nodeId -> PVar nodeId . unqualified <$> identifier

parseLitPattern :: Parser Pattern
parseLitPattern = withNodeId $ \nodeId -> (PLit nodeId) <$> parseLit

parseQualifiedName :: Parser [Text]
parseQualifiedName = sepBy1 identifier dotNoSpaces
