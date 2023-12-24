{-# LANGUAGE OverloadedStrings #-}

module Test.TOML.Array (tests) where

import TOML.Class (Value (..))
import Test.TOML.Common (float1, float2, float3, int1, int2, int3, localTime1, shouldFailParsingArray, shouldParseArray, zonedTime1)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Array"
    [ testCase "parse array of basic types" $ do
        shouldParseArray "[]" []
        shouldParseArray "[1,2,3]" [int1, int2, int3]
        shouldParseArray "[1.0,2.0,3.0]" [float1, float2, float3]
        shouldParseArray "[true,false]" [Bool True, Bool False]
        shouldParseArray "[1979-05-27T07:32:00Z,1979-05-27T07:32:00]" [zonedTime1, localTime1],
      testCase "parse array of arrays" $ do
        shouldParseArray "[[1,2,3], [1,2,3]]" [Array [int1, int2, int3], Array [int1, int2, int3]],
      testCase "parse multi-line arrays" $ do
        shouldParseArray "[\n1\n,2,\n3\n]" [int1, int2, int3]
        shouldParseArray "[1\n,2\n,3]" [int1, int2, int3],
      testCase "allow trailing comma" $ do
        shouldParseArray "[1,2,3,]" [int1, int2, int3],
      testCase "ignore whitespaces" $ do
        shouldParseArray "[1,   2   ,    3]" [int1, int2, int3],
      testCase "ignore comments" $ do
        shouldParseArray "[\n1,\n2, # this is ok\n]" [int1, int2]
        shouldParseArray "[1,2, # this is ok\n# this is ok\n\n3\n]" [int1, int2, int3],
      testCase "fail when the array is not surrounded by brackets" $ do
        shouldFailParsingArray "1,2,3"
        shouldFailParsingArray "[1,2,3"
        shouldFailParsingArray "1,2,3]",
      testCase "fail on missing commas" $ do
        shouldFailParsingArray "[1 2 3]"
        shouldFailParsingArray "[1,2 3]"
    ]
