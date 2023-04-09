{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import Data.Text (Text, pack)
import Data.List
import Data.Function
import qualified Data.IntMap as IM

import Text.Megaparsec hiding (ParseError, State)
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr
import Control.Monad.Reader
import Control.Monad.State

import Error.Diagnose

import Lexer
import Syntax
import Type
import Kind
import NodeId

-- Parses a single file/module
parse :: (FilePath, [FilePath], Text) -> State ParserState (Either ParseError BaseModule)
parse (filePath, modPath, input) = do
    let operatorDefs = [] -- TODO
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


-- Parsing

parseModule :: [Text] -> Parser BaseModule
parseModule modPath = do
    imports <- many (try (parseImport <* newline <* scn))
    exports <- option [] (symbol "export" *> sepBy1 parseExportItem comma <* newline <* scn)

    parsedDecls <- manyTill parseDecl eof
    
    return (Module modPath imports exports parsedDecls)
    where
        parseImport = withNodeId $ \nodeId -> do
            symbol "import"
            Import nodeId <$> sepBy1 identifier (symbol "::")

        parseExportItem = parseModExport <|> parseTypeExport <|> parseDeclExport
        parseDeclExport = withNodeId $ \nodeId -> ExportDecl nodeId <$> identifier
        parseModExport = withNodeId $ \nodeId -> do
            symbol "module"
            ExportMod nodeId <$> sepBy1 identifier (symbol "::")
        parseTypeExport = withNodeId $ \nodeId -> do
            symbol "type"
            ExportType nodeId <$> identifier            

parseDecl :: Parser BaseDecl
parseDecl = (desugarFnDecl <$> parseFnDecl) <|> parseLetDecl

-- Desugars to a let decl + lambda (TODO: patterns)
parseFnDecl :: Parser FnDecl
parseFnDecl = try parseFnWithTypeAnn <|> try parseFnIndented <|> parseFnBasic
    where
        parseFnName = symbol "fn" *> identifier
        parseFnBranch = (,) <$> some identifier <*> (symbol "=" *> parseExpr)

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

-- TODO
desugarFnDecl :: FnDecl -> BaseDecl
desugarFnDecl (FnDecl nodeId fnName typeAnn ((params, expr) : [])) =
    let lambda = foldr (BaseELambda nodeId) expr params in
        DLetDecl nodeId fnName typeAnn lambda
desugarFnDecl (FnDecl _ _ _ []) = error "(?) desugarFnDecl called with empty branch list"
desugarFnDecl _ = error "(!) fn declarations don't support more than one branch yet"

parseLetDecl :: Parser BaseDecl
parseLetDecl = withNodeId $ \nodeId -> do
    symbol "let"
    name <- identifier
    typeAnn <- optional parseTypeAnn
    symbol "="
    DLetDecl nodeId name typeAnn <$> parseExpr
    
parseExpr :: Parser BaseExpr
parseExpr = do
    opers <- ask

    let table = mkTable opers
    makeExprParser parseTerm table
    where
        mkTable = map (map toParser) . groupBy ((==) `on` prec) . sortBy (flip compare `on` prec)
        toParser (OperatorDef assoc _ oper) =
            case assoc of
                ANone -> infixOp oper (flip eBinOp oper)
                ALeft -> infixlOp oper (flip eBinOp oper)
                ARight -> infixrOp oper (flip eBinOp oper)
                APrefix -> prefixOp oper (flip eUnaOp oper)
                APostfix -> postfixOp oper (flip eUnaOp oper)
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
    return (BaseELetExpr nodeId name expr body)

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
parseValue = parseLambda <|> parseLitExpr <|> try parseVariable <|> parensExpr
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
    
    return (foldr (BaseELambda nodeId) expr params)

parseVariable :: Parser BaseExpr
parseVariable = withNodeId $ \nodeId -> BaseEVar nodeId <$> (identifier <|> parens operator)

parseLit :: Parser Lit
parseLit = try (LFloat <$> signed float) <|> (LInt <$> integer)
    <|> (LChar <$> charLiteral) <|> (LString . pack <$> stringLiteral)
    <|> (LBool True <$ symbol "true") <|> (LBool False <$ symbol "false")
    <|> (LUnit <$ symbol "()")
    where
        integer = signed decimal -- <|> try octal <|> try binary <|> hexadecimal

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
        (fnType : typeArgs) -> do
            -- Fix the kinds (since all TCon/TVar kinds are parsed as KStar)
            let k = foldl1 KArrow (map (const KStar) types)
            let fnType' =
                    case fnType of
                        TCon nodeId (TC _ name _) -> TCon nodeId (TC [] name k) -- Default namespace for concrete types is just []
                        TVar (TV name _) -> TVar (TV name k)
                        t@TApp {} -> t -- Parsed TApps should have the correct kind
            return (foldl1 (.) (flip TApp <$> reverse typeArgs) fnType')
        [] -> error "(?) parseTypeApp unreachable case"

parseBaseType :: Parser Type
parseBaseType = parseConcreteType <|> parseTypeVar <|> parens parseType

parseConcreteType :: Parser Type
parseConcreteType = userDefined <|> parsePrimType
    where
        userDefined = withNodeId $
            \nodeId -> TCon nodeId . flip (TC []) KStar <$> typeIdentifier

parsePrimType :: Parser Type
parsePrimType = withNodeId $
    \nodeId -> TCon nodeId . flip (TC []) KStar <$> choice (map symbol primTypes)

parseTypeVar :: Parser Type
parseTypeVar = TVar . flip TV KStar <$> identifier

parsePattern :: Parser Pattern
parsePattern = parens parsePattern <|> parseWildPattern <|> parseVarPattern <|> parseLitPattern

parseWildPattern :: Parser Pattern
parseWildPattern = PWild <$ symbol "_"

parseVarPattern :: Parser Pattern
parseVarPattern = PVar <$> identifier

parseLitPattern :: Parser Pattern
parseLitPattern = PLit <$> parseLit
