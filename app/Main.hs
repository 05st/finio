module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Either
import Data.Functor
import Data.List

import Control.Monad
import Control.Monad.State

import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec

import Options.Applicative
import System.Directory
import System.FilePath

import Lexer hiding (Parser)
import Parser

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
            cprFilePaths -> do
                putStrLn ("INFO: " ++ show (length cprFilePaths) ++ " .fn file(s) found under " ++ path)
                traverse T.readFile cprFilePaths <&> zip cprFilePaths
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

    let toParse = traverse parse (zip3 paths modPaths inputs)
    let (parseRes, parserState) = runState toParse defaultParserState
    
    let parseErrors = lefts parseRes
    if not (null parseErrors)
        then mapM_ reportParseError parseErrors
        else print (rights parseRes)

    where
        defaultParserState = ParserState { curNodeId = 0, posMap = mempty }
        reportParseError e = do
            let diag = errorDiagnosticFromBundle Nothing ("Parse error" :: String) Nothing e
                diag' = addFile diag src (T.unpack "")
            printDiagnostic stderr True True 4 defaultStyle diag'
