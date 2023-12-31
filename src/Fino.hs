module Fino where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Either
import Data.Functor
import Data.List

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec

import Text.Megaparsec hiding (parse)

import Options.Applicative
import System.Directory
import System.FilePath

import Lexer hiding (Parser)
import Parser
import ProgramSort
import AnalysisError
import Resolver
import Infer

data Options = Options
    { src :: FilePath
    , out :: FilePath
    , file :: Bool
    }
    
options :: Parser Options
options = Options
    <$> argument str (value "./" <> metavar "PATH")
    <*> strOption (long "out" <> short 'o' <> value ('a' : exeExtension) <> metavar "FILE" <> help "Output path")
    <*> switch (long "file" <> short 'f' <> help "Input path is to file")

main :: IO ()
main = runOptions =<< execParser (options `withInfo` infoString)
    where
        withInfo opts desc = info (helper <*> opts) (progDesc desc)
        infoString = "Fino Compiler"
        
readDir :: FilePath -> IO [(FilePath, T.Text)]
readDir path = do
    filePaths <- map (path ++) <$> listDirectory path
    readInputs <-
        case filter ((== ".fn") . takeExtension) filePaths of
            [] -> [] <$ putStrLn ("INFO: No .fn files found under " ++ path)
            fnFilePaths -> do
                putStrLn ("INFO: " ++ show (length fnFilePaths) ++ " .fn file(s) found under " ++ path)
                traverse T.readFile fnFilePaths <&> zip fnFilePaths
    readInputsFromChildDirs <- concat <$> (do
        childDirs <- filterM doesDirectoryExist filePaths
        traverse (readDir . (++ ['/'])) childDirs)
    return (readInputs ++ readInputsFromChildDirs)

runOptions :: Options -> IO ()
runOptions (Options src out isFile) = do
    (paths, inputs) <-
        unzip <$> do
            if isFile
                then (:[]) . (src, ) <$> T.readFile src
                else readDir src

    let modPaths = map ((\\ splitDirectories src) . splitDirectories . dropExtension) paths
    
    let toParseOperDefs = zipWithM (runParserT (runReaderT parseModuleOperDefs [])) paths inputs
        operDefs = (concat . rights) (evalState toParseOperDefs defaultParserState)

    let toParse = traverse (parse operDefs) (zip3 paths modPaths inputs)
        (parseRes, parserState) = runState toParse defaultParserState
        posMap' = posMap parserState
    
    case customParseError parserState of
        Just e -> reportAnalysisError posMap' e
        Nothing -> do
            let parseErrors = lefts parseRes
            if not (null parseErrors)
                then mapM_ reportParseError parseErrors
                else let program = rights parseRes in
                    case sortProgram program  >>= resolveProgram >>= inferProgram of
                        Right res -> print res
                        Left e -> reportAnalysisError posMap' e

    where
        defaultParserState = ParserState { curNodeId = 0, posMap = mempty, customParseError = Nothing }

        reportParseError e = do
            let posState = bundlePosState e
                errPath = sourceName (pstateSourcePos posState)
            errSrc <- readFile errPath
            
            let diag = errorDiagnosticFromBundle Nothing ("Parse error" :: String) Nothing e
                diag' = addFile diag errPath errSrc

            printDiagnostic stderr True True 4 defaultStyle diag'

        reportAnalysisError posMap e = do
            diags <- createDiagnostics posMap e
            mapM_ (printDiagnostic stderr True True 4 defaultStyle) diags
