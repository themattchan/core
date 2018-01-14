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
    forM_ [ "ex1_21", "infix_simpl", "infixops"] $ \f -> do
      src <- readFile (testFile f)
      let result = Parser.parseProgram src
      assertBool ("PARSE ERROR: " ++ f) (isRight result)

templateTests =
  testCase "template instantiation" $ do
    src <- readFile $ testFile "ex2_4"
    Template.reduceToNormalForm src @?= Right (Template.NNum 3)
