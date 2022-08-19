module Main (main) where

import System.Directory

import Options.Applicative

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
    print src
    print out
    print file
