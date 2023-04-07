{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import Data.Text (Text, pack)
import Data.Functor.Identity (runIdentity)
import Data.List
import Data.Function

import Text.Megaparsec hiding (ParseError)
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr
import Control.Monad.Reader

import Lexer
import Syntax
import Type

parse :: FilePath -> Text -> Either ParseError BaseDecl
parse file input = runIdentity (runParserT (runReaderT parseDecl []) file input)

parseDecl :: Parser BaseDecl
parseDecl = (desugarFnDecl <$> parseFnDecl) <|> parseLetDecl

-- Desugars to a let decl + lambda (TODO: patterns)
parseFnDecl :: Parser FnDecl
parseFnDecl = try parseFnWithTypeAnn <|> try parseFnIndented <|> parseFnBasic
    where
        parseFnName = symbol "fn" *> identifier
        parseFnBranch = (,) <$> some identifier <*> (symbol "=" *> parseExpr)

        -- Indented function with type annotation
        parseFnWithTypeAnn = indentBlock $ do
            fnName <- parseFnName
            typeAnn <- Just <$> parseTypeAnn
            return (L.IndentSome Nothing (return . FnDecl fnName typeAnn) parseFnBranch)

        -- Indented function without type annotation
        parseFnIndented = indentBlock $ do
            fnName <- parseFnName
            return (L.IndentSome Nothing (return . FnDecl fnName Nothing) parseFnBranch)

        -- No indentation (can't have type annotation, and only one branch)
        parseFnBasic = do
            fnName <- parseFnName
            FnDecl fnName Nothing <$> ((:[]) <$> parseFnBranch)

-- TODO
desugarFnDecl :: FnDecl -> BaseDecl
desugarFnDecl (FnDecl fnName typeAnn ((params, expr) : _)) =
    let lambda = BaseELambda params expr in
        DLetDecl (LetDecl fnName typeAnn lambda)
desugarFnDecl _ = error "(?) desugarFnDecl called with empty branch list"

parseLetDecl :: Parser BaseDecl
parseLetDecl = do
    symbol "let"
    name <- identifier
    typeAnn <- optional parseTypeAnn
    symbol "="
    decl <- LetDecl name typeAnn <$> parseExpr
    return (DLetDecl decl)
    
parseExpr :: Parser BaseExpr
parseExpr = do
    opers <- ask

    let table = mkTable opers
    makeExprParser parseTerm table
    where
        mkTable = map (map toParser) . groupBy ((==) `on` prec) . sortBy (flip compare `on` prec)
        toParser (OperatorDef assoc _ oper) = case assoc of
            ANone -> infixOp oper (EBinOp oper)
            ALeft -> infixlOp oper (EBinOp oper)
            ARight -> infixrOp oper (EBinOp oper)
            APrefix -> prefixOp oper (EUnaOp oper)
            APostfix -> postfixOp oper (EUnaOp oper)
        infixOp name f = InfixN (f <$ symbol name)
        infixlOp name f = InfixL (f <$ symbol name)
        infixrOp name f = InfixR (f <$ symbol name)
        prefixOp name f = Prefix (f <$ symbol name)
        postfixOp name f = Postfix (f <$ symbol name)

parseTerm :: Parser BaseExpr
parseTerm = parseIfExpr <|> parseLetExpr <|> parseFnApp

parseIfExpr :: Parser BaseExpr
parseIfExpr = do
    symbol "if"
    cond <- parseExpr
    symbol "then"
    onTrue <- parseExpr
    onFalse <- option (BaseELit LUnit) (symbol "else" *> parseExpr)
    return (BaseEIfExpr (IfExpr cond onTrue onFalse))

parseLetExpr :: Parser BaseExpr
parseLetExpr = do
    symbol "let"
    name <- identifier
    symbol "="
    expr <- parseExpr
    symbol "in"
    body <- parseExpr
    return (BaseELetExpr (LetExpr name expr body))

parseFnApp :: Parser BaseExpr
parseFnApp = do
    exprs <- some parseValue
    case exprs of
        [a] -> return a
        fn : args -> return (foldl1 (.) (flip BaseEApp <$> reverse args) fn)
        [] -> error "(?) parseFnApp unreachable case"

parseValue :: Parser BaseExpr
parseValue = (BaseELit <$> parseLit) <|> try parseVariable <|> parensExpr
    where
        parensExpr = parens (do
            expr <- parseExpr
            option expr (BaseETypeAnn expr <$> parseTypeAnn)) -- Type annotated expression

parseVariable :: Parser BaseExpr
parseVariable = BaseEVar <$> (identifier <|> parens operator)

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
parseArrowType = do
    paramType <- parseTypeApp
    symbol "->"
    TApp (TApp tArrow paramType) <$> parseType

parseTypeApp :: Parser Type
parseTypeApp = do
    types <- some parseBaseType
    case types of
        [a] -> return a
        (fnType : typeArgs) -> do
            -- Fix the kinds (since all TCon/TVar kinds are parsed as KStar)
            let kind = foldl1 KArrow (map (const KStar) types)
            let fnType' =
                    case fnType of
                        TCon (TC name _) -> TCon (TC name kind)
                        TVar (TV name _) -> TVar (TV name kind)
                        t@TApp {} -> t -- Parsed TApps should have the correct kind
            return (foldl1 (.) (flip TApp <$> reverse typeArgs) fnType')
        [] -> error "(?) parseTypeApp unreachable case"

parseBaseType :: Parser Type
parseBaseType = parseConcreteType <|> parseTypeVar <|> parens parseType

parseConcreteType :: Parser Type
parseConcreteType = userDefined <|> parsePrimType
    where
        userDefined = TCon . flip TC KStar <$> typeIdentifier

parsePrimType :: Parser Type
parsePrimType = TCon . flip TC KStar <$> choice (map symbol
    ["i32", "i64", "f32", "f64", "char", "str", "bool", "unit"])

parseTypeVar :: Parser Type
parseTypeVar = TVar . flip TV KStar <$> identifier
