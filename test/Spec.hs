import Test.Hspec
import Test.Hspec.Golden

import Control.Monad

import System.IO
import System.IO.Silently
import System.Directory
import System.FilePath

import qualified Fino as F

goldenTest :: String -> String -> Golden String
goldenTest name out =
    Golden {
        output = out,
        encodePretty = show,
        writeToFile = writeFile,
        readFromFile = readFile,
        goldenFile = ".tests" </> name </> "expected",
        actualFile = Just (".tests" </> name </> "actual"),
        failFirstTime = False
    }

basePath :: FilePath
basePath = "test/cases/"

spec :: Spec
spec = do
    items <- runIO (listDirectory basePath)
    describe "Golden Tests" $ do
        forM_ items $ \path -> do
            before (runTest (basePath ++ path ++ "/")) $ do
                it ("case: " ++ path) $ \res -> do
                    goldenTest
                        ("case-" ++ path)
                        res

runTest :: [Char] -> IO String
runTest path = do
    let opts = F.Options path "" False
    fst <$> hCapture [stderr] (silence (F.runOptions opts))

main :: IO ()
main = hspec spec
