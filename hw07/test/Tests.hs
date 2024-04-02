import EvalTests
import SimplifyTests
import ParseTests
import Test.Tasty

exprTests :: TestTree
exprTests = testGroup "Expr tests" [evalTests, simplifyTests, parseTests]

main :: IO()
main = defaultMain exprTests