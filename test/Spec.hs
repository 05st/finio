import Test.Hspec
import Test.Hspec.Golden

import Text.Megaparsec (many)
import Parser

main :: IO ()
main = do
    hspec parserSpec
    hspec resolverSpec
        
parserSpec :: Spec
parserSpec =
    describe "Parser" $ do
        it "can parse literals" $
            defaultGolden
                "parse-lits"
                (show $ testParse "123 3.14159 'c' \"abc\" true false () " (many parseLit))
        
        it "can parse expressions" $
            defaultGolden
                "parse-exprs"
                (show $ testParse "let p = false in abc xyz 123 (thing : i32) (\\q w -> if p then 12.3 else ())" parseExpr)
        
        it "can parse functions" $
            defaultGolden
                "parse-fns"
                (show $ testParse "fn abc x = x" parseFnDecl)

resolverSpec :: Spec
resolverSpec =
    describe "Resolver" $ do
        it "can resolve names" $
            defaultGolden
                "resolve-names"
                (show 12)
