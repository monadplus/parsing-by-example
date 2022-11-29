{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Data.Function (on)
import Data.Functor (void)
import qualified Data.Text as Text
import qualified Data.Time.Calendar.OrdinalDate as T
import qualified Data.Time.LocalTime as T
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog as Hedgehog
import JSON.Class (JValue)
import qualified JSON.Gen as Gen
import qualified JSON.Lexer as Lexer
import qualified JSON.Parser as Parser
import System.IO (hSetEncoding, stderr, stdout, utf8)
import qualified TOML.Lexer as TOML
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ unitTests,
      propTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ parseZonedTimeTests,
      parseLocalTimeTests,
      parseTimeOfDayTests,
      parseDayTests
    ]
  where
    parseZonedTimeTests =
      testGroup
        "Parse ZonedTime"
        [ testCase "Date 1" $ do
            let date = "1979-05-27T07:32:00Z"
                expected =
                  T.ZonedTime
                    (T.LocalTime (T.YearDay 1979 147) (T.TimeOfDay 7 32 0))
                    (T.hoursToTimeZone 0)
                Just result = TOML.parseZonedTime date
            assertEqualZonedTime date expected result,
          testCase "Date 2" $ do
            let date = "1979-05-27T00:32:00-07:00"
                expected =
                  T.ZonedTime
                    (T.LocalTime (T.YearDay 1979 147) (T.TimeOfDay 0 32 0))
                    (T.hoursToTimeZone (-7))
                Just result = TOML.parseZonedTime date
            assertEqualZonedTime date expected result,
          testCase "Date 3" $ do
            let date = "1979-05-27T00:32:00.999999-07:00"
                expected =
                  T.ZonedTime
                    (T.LocalTime (T.YearDay 1979 147) (T.TimeOfDay 0 32 00.999999))
                    (T.hoursToTimeZone (-7))
                Just result = TOML.parseZonedTime date
            assertEqualZonedTime date expected result,
          testCase "Date 4" $ do
            let date = "1979-05-27 07:32:00Z"
                expected =
                  T.ZonedTime
                    (T.LocalTime (T.YearDay 1979 147) (T.TimeOfDay 7 32 0))
                    (T.hoursToTimeZone 0)
                Just result = TOML.parseZonedTime date
            assertEqualZonedTime date expected result
        ]

    parseLocalTimeTests =
      testGroup
        "Parse LocalTime"
        [ testCase "Date 1" $ do
            let date = "1979-05-27T07:32:00"
                expected =
                  Just $
                    T.LocalTime (T.YearDay 1979 147) (T.TimeOfDay 7 32 0)
            assertEqual date expected (TOML.parseLocalTime date),
          testCase "Date 2" $ do
            let date = "1979-05-27T00:32:00.999999"
                expected =
                  Just $
                    T.LocalTime (T.YearDay 1979 147) (T.TimeOfDay 0 32 00.999999)
            assertEqual date expected (TOML.parseLocalTime date)
        ]

    parseTimeOfDayTests =
      testGroup
        "Parse TimeOfDay"
        [ testCase "Date 1" $ do
            let date = "07:32:00"
                expected = Just $ T.TimeOfDay 7 32 0
            assertEqual date expected (TOML.parseTimeOfDay date),
          testCase "Date 2" $ do
            let date = "00:32:00.999999"
                expected = Just $ T.TimeOfDay 0 32 00.999999
            assertEqual date expected (TOML.parseTimeOfDay date)
        ]

    parseDayTests =
      testGroup
        "Parse Day"
        [ testCase "Date 1" $ do
            let date = "1979-05-27"
                expected = Just $ T.YearDay 1979 147
            assertEqual date expected (TOML.parseDay date)
        ]

assertEqualZonedTime :: String -> T.ZonedTime -> T.ZonedTime -> Assertion
assertEqualZonedTime date = assertEqual date `on` T.zonedTimeToUTC

propTests :: TestTree
propTests =
  testGroup
    "Property-based Tests"
    [ testGroup
        "Lexing"
        [ testPropertyNamed "lexer jstring" "prop_lexer_jstring" prop_lexer_jstring,
          testPropertyNamed "lexer jnumber" "prop_lexer_jnumber" prop_lexer_jnumber,
          testPropertyNamed "lexer jarray" "prop_lexer_jarray" prop_lexer_jarray,
          testPropertyNamed "lexer jarray" "prop_lexer_jarray_ws" prop_lexer_jarray_ws,
          testPropertyNamed "lexer jobject" "prop_lexer_jobject" prop_lexer_jobject,
          testPropertyNamed "lexer jobject" "prop_lexer_jobject_ws" prop_lexer_jobject_ws,
          testPropertyNamed "lexer jvalue" "prop_lexer_jvalue" prop_lexer_jvalue,
          testPropertyNamed "lexer jvalue" "prop_lexer_jvalue_ws" prop_lexer_jvalue_ws
        ],
      testGroup
        "Parsing"
        [ testPropertyNamed "parser jstring" "prop_parser_jstring" prop_parser_jstring,
          testPropertyNamed "parser jnumber" "prop_parser_jnumber" prop_parser_jnumber,
          testPropertyNamed "parser jarray" "prop_parser_jarray" prop_parser_jarray,
          testPropertyNamed "parser jarray" "prop_parser_jarray_ws" prop_parser_jarray_ws,
          testPropertyNamed "parser jobject" "prop_parser_jobject" prop_parser_jobject,
          testPropertyNamed "parser jobject" "prop_parser_jobject_ws" prop_parser_jobject_ws,
          testPropertyNamed "parser jvalue" "prop_parser_jvalue" prop_parser_jvalue,
          testPropertyNamed "parser jvalue" "prop_parser_jvalue_ws" prop_parser_jvalue_ws
        ]
    ]
  where
    prop_lexer_jstring = prop_lexer_generic Gen.jStringGen
    prop_lexer_jnumber = prop_lexer_generic Gen.jNumberGen
    prop_lexer_jarray = prop_lexer_generic Gen.jArrayGen
    prop_lexer_jarray_ws = prop_lexer_stringify Gen.jArrayGen
    prop_lexer_jobject = prop_lexer_generic Gen.jObjectGen
    prop_lexer_jobject_ws = prop_lexer_stringify Gen.jObjectGen
    prop_lexer_jvalue = prop_lexer_generic Gen.jValueGen
    prop_lexer_jvalue_ws = prop_lexer_stringify Gen.jValueGen

    prop_parser_jstring = prop_parser_generic Gen.jStringGen
    prop_parser_jnumber = prop_parser_generic Gen.jNumberGen
    prop_parser_jarray = prop_parser_generic Gen.jArrayGen
    prop_parser_jarray_ws = prop_parser_stringify Gen.jArrayGen
    prop_parser_jobject = prop_parser_generic Gen.jObjectGen
    prop_parser_jobject_ws = prop_parser_stringify Gen.jObjectGen
    prop_parser_jvalue = prop_parser_generic Gen.jValueGen
    prop_parser_jvalue_ws = prop_parser_stringify Gen.jValueGen

prop_lexer_generic :: Gen JValue -> Property
prop_lexer_generic gen = property $ do
  jValue <- forAll gen
  let r = Lexer.lex "" (Text.pack $ show jValue)
  void $ Hedgehog.evalEither $ r

prop_lexer_stringify :: Gen JValue -> Property
prop_lexer_stringify gen = property $ do
  jValue <- forAll gen
  stringified <- forAll (Gen.stringify jValue)
  let r = Lexer.lex "" stringified
  void $ Hedgehog.evalEither $ r

prop_parser_generic :: Gen JValue -> Property
prop_parser_generic gen = property $ do
  jValue <- forAll gen
  case Parser.parse "" (Text.pack $ show jValue) of
    Left err -> do
      Hedgehog.annotate err
      Hedgehog.failure
    Right jValue' -> jValue' === jValue

prop_parser_stringify :: Gen JValue -> Property
prop_parser_stringify gen = property $ do
  jValue <- forAll gen
  stringified <- forAll (Gen.stringify jValue)
  case Parser.parse "" stringified of
    Left err -> do
      Hedgehog.annotate err
      Hedgehog.failure
    Right jValue' -> jValue' === jValue
