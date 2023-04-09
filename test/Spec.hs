import Test.Hspec
import Test.Hspec.Golden

import Control.Monad

import System.IO
import System.IO.Silently
import System.Directory

import qualified Fino as F

basePath :: FilePath
basePath = "test/cases/"

spec :: Spec
spec = do
    items <- runIO (listDirectory basePath)
    forM_ items $ \path -> do
        before (runTest (basePath ++ path ++ "/")) $ describe "" $ do
            it ("case: " ++ path) $ \res -> do
                defaultGolden
                    ("case-" ++ path)
                    res

runTest :: [Char] -> IO String
runTest path = do
    let opts = F.Options path "" False
    fst <$> hCapture [stderr] (F.runOptions opts)

main :: IO ()
main = hspec spec
