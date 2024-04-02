module ParseTests where

import Test.Tasty
import Test.Tasty.HUnit
import Expr
import Parser (runExprParser)


parseTests :: TestTree
parseTests = testGroup "Parse tests"
  [ testCase "base number test" $ assertEqual "expr of number" (runExprParser "123") (Right (Expr 123.0))
    , testCase "base variable test" $ assertEqual "expr of variable" (runExprParser "xyz") (Right (Var "xyz"))
    , testCase "sqrt variable test" $ assertEqual "sqrt of variable" (runExprParser "sqrt xyz") (Right (Sq (Var "xyz")))
    , testCase "sqrt number test" $ assertEqual "sqrt of number" (runExprParser "sqrt 16") (Right (Sq (Expr 16.0)))
    , testCase "number op test" $ assertEqual "op on numbers" (runExprParser "+ 123 256") (Right ((:+) (Expr 123) (Expr 256) ))
    , testCase "var op test" $ assertEqual "op on vars" (runExprParser "+ xyz pqr") (Right ((:+) (Var "xyz") (Var "pqr") ))
    , testCase "var and number op test" $ assertEqual "op on var and number" (runExprParser "+ 123 xyz") (Right ((:+) (Expr 123) (Var "xyz") ))
  ]