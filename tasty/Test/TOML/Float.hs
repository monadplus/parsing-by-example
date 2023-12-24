{-# LANGUAGE OverloadedStrings #-}

module Test.TOML.Float (tests) where

import Test.TOML.Common (shouldFailParsingFloat, shouldParseFloat)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Float"
    [ fractionalTests,
      exponentTests,
      fractionalExponentTests,
      invalidFloatTests,
      underscoreTests,
      infNanTests
    ]

fractionalTests :: TestTree
fractionalTests =
  testGroup
    "Fractional"
    [ testCase "Parse +1.0" $
        shouldParseFloat "+1.0" 1.0,
      testCase "Parse 3.1415" $
        shouldParseFloat "3.1415" 3.1415,
      testCase "Parse -0.01" $
        shouldParseFloat "-0.01" (-0.01)
    ]

exponentTests :: TestTree
exponentTests =
  testGroup
    "Exponent"
    [ testCase "Parse 5e+22" $
        shouldParseFloat "5e+22" 5e+22,
      testCase "Parse 1e06" $
        shouldParseFloat "1e06" 1e06,
      testCase "Parse -2E-2" $
        shouldParseFloat "-2E-2" (-2E-2)
    ]

fractionalExponentTests :: TestTree
fractionalExponentTests =
  testGroup
    "Fractional - Exponent"
    [ testCase "Parse 6.626e-34" $
        shouldParseFloat "6.626e-34" 6.626e-34
    ]

invalidFloatTests :: TestTree
invalidFloatTests =
  testGroup
    "Invalid "
    [ testCase "Fail on .7" $
        shouldFailParsingFloat ".7",
      testCase "Fail on 7." $
        shouldFailParsingFloat "7.",
      testCase "Fail on 3.e+20" $
        shouldFailParsingFloat "3.e+20"
    ]

underscoreTests :: TestTree
underscoreTests =
  testGroup
    "Underscore"
    [ testCase "Parse 224_617.445_991_228" $
        shouldParseFloat "224_617.445_991_228" 224617.445991228
    ]

infNanTests :: TestTree
infNanTests =
  testGroup
    "Inf/Nan"
    [ testCase "Parse inf" $
        shouldParseFloat "inf" (1 / 0),
      testCase "Parse +inf" $
        shouldParseFloat "+inf" (1 / 0),
      testCase "Parse -inf" $
        shouldParseFloat "-inf" (-(1 / 0)),
      testCase "Parse nan" $
        shouldParseFloat "nan" (0 / 0),
      testCase "Parse +nan" $
        shouldParseFloat "+nan" (0 / 0),
      testCase "Parse -nan" $
        shouldParseFloat "-nan" (-(0 / 0))
    ]
