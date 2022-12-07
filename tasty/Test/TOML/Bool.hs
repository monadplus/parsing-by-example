{-# LANGUAGE OverloadedStrings #-}

module Test.TOML.Bool (tests) where

import Test.TOML.Common (shouldFailParsingBool, shouldParseBool)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Bool"
    [ testCase "Parse true" $
        shouldParseBool "true" True,
      testCase "Parse false" $
        shouldParseBool "false" False,
      testCase "Fail on True" $
        shouldFailParsingBool "True",
      testCase "Fail on False" $
        shouldFailParsingBool "False"
    ]
