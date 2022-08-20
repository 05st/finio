{-# Language OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as  T
import qualified Data.Text.IO as T

import System.Directory

import Options.Applicative

import Parser

import Error.Diagnose 
import Error.Diagnose.Compat.Megaparsec

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

runOptions :: Options -> IO ()
runOptions (Options src out file) = do
    input <- T.readFile src
    case parse src input of
        Left err -> do
            let diagnostic = errorDiagnosticFromBundle Nothing ("Parse error on input" :: String) Nothing err
                diagnostic' = addFile diagnostic src (T.unpack input)
            printDiagnostic stderr True True 4 defaultStyle diagnostic'
        Right top -> print top
