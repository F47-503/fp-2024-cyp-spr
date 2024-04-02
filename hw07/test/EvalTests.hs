module EvalTests where

import Test.Tasty
import Test.Tasty.HUnit
import Eval
import Expr
import qualified Data.Map.Strict as Map


evalTests :: TestTree
evalTests = testGroup "Eval tests"
  [ testCase "base test" $ assertEqual "eval on number is number" (runEval (Expr 1.0) Map.empty) (Right 1.0)
  , testCase "Sq test" $ assertEqual "Sq must be square root" (runEval (Sq (Expr 4.0)) Map.empty) (Right 2.0)
  , testCase "base Error" $ assertEqual "taking sqrt of negative must fail the whole calculation" (runEval (Expr 1.0 :+ Sq (Expr (-4.0))) Map.empty) (Left (Error "sqrt from negative taken"))
  , testCase "base operations" $ assertEqual "eval must calculate expressions" (runEval ((Expr 2.0 :^ Expr 3.0) :+ ((Expr 2.0 :* Expr 3.0) :- (Expr 4.0 :/ Expr 1.0))) Map.empty) (Right 10.0)
  , testCase "multiple errors" $ assertEqual "from multiple errors we keep first in recursion" (runEval ((Expr 1.0 :/ Expr 0.0) :+ Sq (Expr (-1.0))) Map.empty) (Left (Error "divided by zero"))
  , testCase "negative base error" $ assertEqual "no negative base allowed" (runEval (Expr (-1.0) :^ Expr 3.0) Map.empty) (Left (Error "power used with non-positive base"))
  , testCase "zero base error" $ assertEqual "base must be greater than zero" (runEval (Expr 0.0 :^ Expr (-3.0)) Map.empty) (Left (Error "power used with non-positive base"))
  , testCase "base variable substitution" $ assertEqual "eval must substitute variable" (runEval (Var "x") (Map.fromAscList [("x", 8.0)])) (Right 8.0)
  , testCase "variable substitution without value" $ assertEqual "eval must fail if variable is unknown" (runEval (Var "x") Map.empty) (Left (Error "Unknown variable: x"))
  -- this test is commented due to Map nature whcih does not allow multiple keys.
  --, testCase "variable substitution with too many values" $ assertEqual "eval must fail for multiple definitions of variable" (eval (Var "x") [("x", 2), ("x", 3)]) (Left (Error "Non-unique variable value: x"))
  , testCase "base variable substitution with other error" $ assertEqual "eval with variables cannot avoid errors" (runEval (Var "x" :+ Sq (Expr (-1.0))) (Map.fromAscList [("x", 8.0)])) (Left (Error "sqrt from negative taken"))
  ]