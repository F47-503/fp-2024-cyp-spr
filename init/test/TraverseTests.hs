module TraverseTests where

import Test.Tasty
import Test.Tasty.HUnit
import Exam
import Text.Printf (FormatAdjustment(LeftAdjust))

sampleTree = Node (Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)) 1 (Node (Node Leaf 6 Leaf) 3 (Node Leaf 7 Leaf))

traverseTests :: TestTree
traverseTests = testGroup "traverse tests"
    [
        testCase "NLR" $ nlr sampleTree @?= reverse [1,2,4,5,3,6,7]
        , testCase "LNR" $ lnr sampleTree @?= reverse [4,2,5,1,6,3,7]
        , testCase "LRN" $ lrn sampleTree @?= reverse [4,5,2,6,7,3,1]
    ]