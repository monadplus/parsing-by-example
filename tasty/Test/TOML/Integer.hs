{-# LANGUAGE OverloadedStrings #-}

module Test.TOML.Integer (tests) where

import Test.TOML.Common (shouldFailParsingInteger, shouldParseInteger)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Integer"
    [ decimalTests,
      hexadecimalTests,
      octalTests,
      binaryTests
    ]

decimalTests :: TestTree
decimalTests =
  testGroup
    "Decimal"
    [ testCase "Parse 42" $
        shouldParseInteger "42" 42,
      testCase "Parse +99" $
        shouldParseInteger "+99" 99,
      testCase "Parse +0" $
        shouldParseInteger "+0" 0,
      testCase "Parse -0" $
        shouldParseInteger "-0" (-0),
      testCase "Parse -17" $
        shouldParseInteger "-17" (-17),
      testCase "Parse 1_000" $
        shouldParseInteger "1_000" 1000,
      testCase "Parse 1_2_3_4_5" $
        shouldParseInteger "1_2_3_4_5" 12345,
      testCase "Fail on 123__45" $
        shouldFailParsingInteger "123__45",
      testCase "Fail on 01" $
        shouldFailParsingInteger "01234",
      testCase "Fail on 001" $
        shouldFailParsingInteger "001",
      testCase "Fail on +01" $
        shouldFailParsingInteger "+01",
      testCase "Fail on -01" $
        shouldFailParsingInteger "-01"
    ]

hexadecimalTests :: TestTree
hexadecimalTests =
  testGroup
    "Hexadecimal"
    [ testCase "Parse 0xDEADBEEF" $ do
        shouldParseInteger "0xDEADBEEF" 3735928559,
      testCase "Parse 0xdeadbeef" $ do
        shouldParseInteger "0xdeadbeef" 3735928559,
      testCase "Parse 0xdead_beef" $ do
        shouldParseInteger "0xdead_beef" 3735928559,
      testCase "Fail on 0x_DEADBEEF" $ do
        shouldFailParsingInteger "0x_DEADBEEF"
    ]

octalTests :: TestTree
octalTests =
  testGroup
    "Octal"
    [ testCase "Parse 0o01234567" $ do
        shouldParseInteger "0o01234567" 342391,
      testCase "Parse 0o0123_4567" $ do
        shouldParseInteger "0o0123_4567" 342391,
      testCase "Parse 0o755" $ do
        shouldParseInteger "0o755" 493,
      testCase "Parse 0o7__55" $ do
        shouldFailParsingInteger "0o7__55"
    ]

binaryTests :: TestTree
binaryTests =
  testGroup
    "Octal"
    [ testCase "Parse 0b11010110" $ do
        shouldParseInteger "0b11010110" 214,
      testCase "Parse 0b1101_0110" $ do
        shouldParseInteger "0b1101_0110" 214
    ]
