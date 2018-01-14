{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Control.Exception (SomeException, try, catch)
import Data.Either
import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath

import           Core.Language
import qualified Core.Parser as Parser
import qualified Core.Template as Template

testFile :: FilePath -> FilePath
testFile = ("test/input" </>) . (<.> "core")

main :: IO ()
main = defaultMain $
  testGroup "Tests"
    [ parserTests
    , templateTests
    ]

parserTests =
  testCase "parser tests" $ do
    forM_ [ "ex1_21", "infix_simpl", "infixops", "ex2_11"] $ \f -> do
      src <- readFile (testFile f)
      let result = Parser.parseProgram src
      assertBool ("PARSE ERROR: " ++ f) (isRight result)

templateTests =
  testCase "template instantiation" $ do
    let rnfFile f expect = do
          src <- readFile $ testFile f
          Template.reduceToNormalForm src @?= Right expect

    rnfFile "ex2_4" (Template.NNum 3)
    rnfFile "ex2_11" (Template.NNum 4)
