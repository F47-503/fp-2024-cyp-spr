module ParseTests where

import Test.Tasty
import Test.Tasty.HUnit
import Expr
import Parser (runExprParser, ParseError(..))


parseTests :: TestTree
parseTests = testGroup "Parse tests"
  [ testCase "base number test" $ assertEqual "expr of number" (runExprParser "123") (Right (Expr 123.0))
    , testCase "base variable test" $ assertEqual "expr of variable" (runExprParser "xyz") (Right (Var "xyz"))
    , testCase "sqrt variable test" $ assertEqual "sqrt of variable" (runExprParser "sqrt xyz") (Right (Sq (Var "xyz")))
    , testCase "sqrt number test" $ assertEqual "sqrt of number" (runExprParser "sqrt 16") (Right (Sq (Expr 16.0)))
    , testCase "number op test" $ assertEqual "op on numbers" (runExprParser "+ 123 256") (Right ((:+) (Expr 123) (Expr 256) ))
    , testCase "var op test" $ assertEqual "op on vars" (runExprParser "+ xyz pqr") (Right ((:+) (Var "xyz") (Var "pqr") ))
    , testCase "var and number op test" $ assertEqual "op on var and number" (runExprParser "+ 123 xyz") (Right ((:+) (Expr 123) (Var "xyz") ))
    , testCase "op composition test 1" $ assertEqual "multiple ops at once 1" (runExprParser "+ 123 * 45 6") (Right ((:+) (Expr 123) (Expr 45 :* Expr 6) ))
    , testCase "op composition test 2" $ assertEqual "multiple ops at once 2" (runExprParser "+ * 123 45 6") (Right ((:+) (Expr 123 :* Expr 45) (Expr 6) ))
    , testCase "op composition test with vars and sq" $ assertEqual "multiple ops at once 3" (runExprParser "/ sqrt 123 xyz") (Right ((:/) (Sq (Expr 123)) (Var "xyz")))
    , testCase "empty sqrt" $ assertEqual "parsing sqrt is a error" (runExprParser "sqrt") (Left (ParseError "Parsing ended unexpectedly"))
    , testCase "binary incomplete op" $ assertEqual "binary ops shouldn't work with one arg" (runExprParser "- 123") (Left (ParseError "Parsing ended unexpectedly"))
    , testCase "additional chars at the end" $ assertEqual "extra chars aren't allowed" (runExprParser "123 a") (Left (ParseError "Unexpected continuation 123 a"))
    , testCase "special symbols are not allowed" $ assertEqual "can't use special symbols in variables" (runExprParser "sq?") (Left (ParseError "Unexpected continuation sq?"))    
    , testCase "special symbols are not allowed at all 1" $ assertEqual "can't use special symbols in variables anywhere 1" (runExprParser "+ 123 sq?") (Left (ParseError "Unexpected continuation + 123 sq?")) 
    , testCase "special symbols are not allowed at all 2" $ assertEqual "can't use special symbols in variables anywhere 2" (runExprParser "+ sq? 123 456") (Left (ParseError "Parsing ended unexpectedly"))      
    , testCase "special symbols are not allowed at beginning" $ assertEqual "can't use special symbols in begin of a variable" (runExprParser "+sq") (Left (ParseError "Parsing ended unexpectedly")) 
  ]