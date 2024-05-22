import Test.Tasty
import RotateTests
import LengthCPSTests
import TraverseTests

tests :: TestTree
tests = testGroup "All tests" [rotateTests, lengthCPSTests, traverseTests]

main :: IO()
main = defaultMain tests