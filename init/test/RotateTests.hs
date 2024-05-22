module RotateTests where

import Test.Tasty
import Test.Tasty.HUnit
import Exam

rotateTests :: TestTree
rotateTests = testGroup "rotate tests"
    [
        testCase "no rotate at all" $ rotate 0 [1,2] @?= [1,2]
        , testCase "simple test" $ rotate 2 [1,2,3,4,5] @?= [3,4,5,1,2]
        , testCase "simple test negative" $ rotate (-2) [1,2,3,4,5] @?= [4,5,1,2,3]
        , testCase "infinite test" $ take 10 (rotate 2 [0..]) @?= take 10 [2..]
    ]