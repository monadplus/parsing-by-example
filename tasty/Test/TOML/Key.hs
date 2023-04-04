{-# LANGUAGE OverloadedStrings #-}

module Test.TOML.Key (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import TOML.Class (Key (..))
import Test.TOML.Common (shouldFailParsingKey, shouldFailParsingTableHeader, shouldFailParsingTableHeaderArray, shouldParseKey, shouldParseTableHeader, shouldParseTableHeaderArray)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Key"
    [ testCase "parse bare keys" $ do
        shouldParseKey "key" "key"
        shouldParseKey "bare_key" "bare_key"
        shouldParseKey "bare-key" "bare-key"
        shouldParseKey "1234" "1234"
        shouldFailParsingKey "bare/key",
      testCase "parse quoted keys" $ do
        shouldParseKey "\"character encoding\"" "character encoding"
        shouldParseKey "\"ʎǝʞ\"" "ʎǝʞ"
        shouldParseKey "'key2'" "key2"
        shouldParseKey "'quoted \"value\"'" "quoted \"value\"",
      testCase "parse empty quoted keys" $ do
        shouldParseKey "\"\"" ""
        shouldParseKey "''" "",
      testCase "parse dotted keys" $ do
        shouldParseKey "physical.color" "physical.color"
        shouldParseKey "3.14159" "3.14159"
        shouldParseKey "site.\"google.com\"" (Key $ "site" :| ["google.com"]),
      testCase "ignore whitespaces around dots in dotted keys" $ do
        shouldParseKey "fruit.name" "fruit.name"
        shouldParseKey "fruit. color" "fruit.color"
        shouldParseKey "fruit . flavor" "fruit.flavor",
      testCase "fail on empty bare keys" $ do
        shouldFailParsingKey "",
      testCase "parse table header" $ do
        shouldParseTableHeader "[key]" "key"
        shouldParseTableHeader "[fruit.color]" "fruit.color"
        shouldFailParsingTableHeader "[fruit.color"
        shouldFailParsingTableHeader "fruit.color]",
      testCase "parse table header array" $ do
        shouldParseTableHeaderArray "[[key]]" "key"
        shouldParseTableHeaderArray "[[fruit.color]]" "fruit.color"
        shouldFailParsingTableHeaderArray "[[fruit.color]"
        shouldFailParsingTableHeaderArray "[[fruit.color"
        shouldFailParsingTableHeaderArray "[fruit.color]]"
    ]
