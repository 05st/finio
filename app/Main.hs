module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.IntMap as IM

import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec

import Options.Applicative
import System.Directory

import Parser
import Syntax

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
runOptions (Options src out isFile) = do
    input <- T.readFile src
    case parse src input of
        Left e -> do
            let diag = errorDiagnosticFromBundle Nothing ("Parse error" :: String) Nothing e
                diag' = addFile diag src (T.unpack input)
            printDiagnostic stderr True True 4 defaultStyle diag'
        Right (res, spanMap) -> do
            print res
            let (Just s) = IM.lookup 4 spanMap
            let e = err Nothing ("Type mismatch" :: String) [(spanToPosition s, This "expected 'f32', got 'bool'")] ["here's a hint"]
                diag = addFile def src (T.unpack input)
                diag' = addReport diag e
            printDiagnostic stderr True True 4 defaultStyle diag'
