module LengthCPSTests where

import Test.Tasty
import Test.Tasty.HUnit
import Exam

lengthCPSTests :: TestTree
lengthCPSTests = testGroup "lengthCPS tests"
    [
        testCase "id for simple length" $ lengthCPS [1,2] id @?= 2
        , testCase "empty list" $ lengthCPS [] id @?= 0
        , testCase "add something to answer" $ lengthCPS [1,2,3] (+3) @?= 6
    ]