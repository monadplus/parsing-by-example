{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Data.Functor (void)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Time.Calendar.OrdinalDate as T
import qualified Data.Time.LocalTime as T
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog as Hedgehog
import JSON.Class (JValue)
import qualified JSON.Gen as JSON.Gen
import qualified JSON.Lexer as JSON.Lexer
import qualified JSON.Parser as JSON.Parser
import System.IO (hSetEncoding, stderr, stdout, utf8)
import qualified TOML.Lexer as TOML.Lexer
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
                msg = Text.unpack date
                expected =
                  Right . List.singleton . TOML.Lexer.TimeLit . TOML.Lexer.ZonedTime $
                    T.ZonedTime
                      (T.LocalTime (T.YearDay 1979 147) (T.TimeOfDay 7 32 0))
                      (T.hoursToTimeZone 0)
                result = TOML.Lexer.lex "" date
            assertEqual msg expected result,
          testCase "Date 2" $ do
            let date = "1979-05-27T00:32:00-07:00"
                msg = Text.unpack date
                expected =
                  Right . List.singleton . TOML.Lexer.TimeLit . TOML.Lexer.ZonedTime $
                    T.ZonedTime
                      (T.LocalTime (T.YearDay 1979 147) (T.TimeOfDay 0 32 0))
                      (T.hoursToTimeZone (-7))
                result = TOML.Lexer.lex "" date
            assertEqual msg expected result,
          testCase "Date 3" $ do
            let date = "1979-05-27T00:32:00.999999-07:00"
                msg = Text.unpack date
                expected =
                  Right . List.singleton . TOML.Lexer.TimeLit . TOML.Lexer.ZonedTime $
                    T.ZonedTime
                      (T.LocalTime (T.YearDay 1979 147) (T.TimeOfDay 0 32 00.999999))
                      (T.hoursToTimeZone (-7))
                result = TOML.Lexer.lex "" date
            assertEqual msg expected result,
          testCase "Date 4" $ do
            let date = "1979-05-27 07:32:00Z"
                msg = Text.unpack date
                expected =
                  Right . List.singleton . TOML.Lexer.TimeLit . TOML.Lexer.ZonedTime $
                    T.ZonedTime
                      (T.LocalTime (T.YearDay 1979 147) (T.TimeOfDay 7 32 0))
                      (T.hoursToTimeZone 0)
                result = TOML.Lexer.lex "" date
            assertEqual msg expected result
        ]

    parseLocalTimeTests =
      testGroup
        "Parse LocalTime"
        [ testCase "Date 1" $ do
            let date = "1979-05-27T07:32:00"
                msg = Text.unpack date
                expected =
                  Right . List.singleton . TOML.Lexer.TimeLit . TOML.Lexer.LocalTime $
                    T.LocalTime (T.YearDay 1979 147) (T.TimeOfDay 7 32 0)
                result = TOML.Lexer.lex "" date
            assertEqual msg expected result,
          testCase "Date 2" $ do
            let date = "1979-05-27T00:32:00.999999"
                msg = Text.unpack date
                expected =
                  Right . List.singleton . TOML.Lexer.TimeLit . TOML.Lexer.LocalTime $
                    T.LocalTime (T.YearDay 1979 147) (T.TimeOfDay 0 32 00.999999)
                result = TOML.Lexer.lex "" date
            assertEqual msg expected result
        ]

    parseTimeOfDayTests =
      testGroup
        "Parse TimeOfDay"
        [ testCase "Date 1" $ do
            let date = "07:32:00"
                msg = Text.unpack date
                expected =
                  Right . List.singleton . TOML.Lexer.TimeLit . TOML.Lexer.TimeOfDay $
                    T.TimeOfDay 7 32 0
                result = TOML.Lexer.lex "" date
            assertEqual msg expected result,
          testCase "Date 2" $ do
            let date = "00:32:00.999999"
                msg = Text.unpack date
                expected =
                  Right . List.singleton . TOML.Lexer.TimeLit . TOML.Lexer.TimeOfDay $
                    T.TimeOfDay 0 32 00.999999
                result = TOML.Lexer.lex "" date
            assertEqual msg expected result
        ]

    parseDayTests =
      testGroup
        "Parse Day"
        [ testCase "Date 1" $ do
            let date = "1979-05-27"
                msg = Text.unpack date
                expected =
                  Right . List.singleton . TOML.Lexer.TimeLit . TOML.Lexer.Day $
                    T.YearDay 1979 147
                result = TOML.Lexer.lex "" date
            assertEqual msg expected result
        ]

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
    prop_lexer_jstring = prop_lexer_generic JSON.Gen.jStringGen
    prop_lexer_jnumber = prop_lexer_generic JSON.Gen.jNumberGen
    prop_lexer_jarray = prop_lexer_generic JSON.Gen.jArrayGen
    prop_lexer_jarray_ws = prop_lexer_stringify JSON.Gen.jArrayGen
    prop_lexer_jobject = prop_lexer_generic JSON.Gen.jObjectGen
    prop_lexer_jobject_ws = prop_lexer_stringify JSON.Gen.jObjectGen
    prop_lexer_jvalue = prop_lexer_generic JSON.Gen.jValueGen
    prop_lexer_jvalue_ws = prop_lexer_stringify JSON.Gen.jValueGen

    prop_parser_jstring = prop_parser_generic JSON.Gen.jStringGen
    prop_parser_jnumber = prop_parser_generic JSON.Gen.jNumberGen
    prop_parser_jarray = prop_parser_generic JSON.Gen.jArrayGen
    prop_parser_jarray_ws = prop_parser_stringify JSON.Gen.jArrayGen
    prop_parser_jobject = prop_parser_generic JSON.Gen.jObjectGen
    prop_parser_jobject_ws = prop_parser_stringify JSON.Gen.jObjectGen
    prop_parser_jvalue = prop_parser_generic JSON.Gen.jValueGen
    prop_parser_jvalue_ws = prop_parser_stringify JSON.Gen.jValueGen

prop_lexer_generic :: Gen JValue -> Property
prop_lexer_generic gen = property $ do
  jValue <- forAll gen
  let r = JSON.Lexer.lex "" (Text.pack $ show jValue)
  void $ Hedgehog.evalEither $ r

prop_lexer_stringify :: Gen JValue -> Property
prop_lexer_stringify gen = property $ do
  jValue <- forAll gen
  stringified <- forAll (JSON.Gen.stringify jValue)
  let r = JSON.Lexer.lex "" stringified
  void $ Hedgehog.evalEither $ r

prop_parser_generic :: Gen JValue -> Property
prop_parser_generic gen = property $ do
  jValue <- forAll gen
  case JSON.Parser.parse "" (Text.pack $ show jValue) of
    Left err -> do
      Hedgehog.annotate err
      Hedgehog.failure
    Right jValue' -> jValue' === jValue

prop_parser_stringify :: Gen JValue -> Property
prop_parser_stringify gen = property $ do
  jValue <- forAll gen
  stringified <- forAll (JSON.Gen.stringify jValue)
  case JSON.Parser.parse "" stringified of
    Left err -> do
      Hedgehog.annotate err
      Hedgehog.failure
    Right jValue' -> jValue' === jValue
