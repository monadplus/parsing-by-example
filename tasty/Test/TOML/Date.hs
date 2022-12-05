{-# LANGUAGE OverloadedStrings #-}

module Test.TOML.Date (tests) where

import qualified Data.Time.LocalTime as T
import TOML.Class (Value (..))
import Test.TOML.Common (day1, offset1, offset2, parseDateTime, timeOfDay1, timeOfDay2, timeOfDay3)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Date"
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
            let expected = ZonedTime $ T.ZonedTime (T.LocalTime day1 timeOfDay1) offset1
            parseDateTime "1979-05-27T07:32:00Z" expected,
          testCase "Date 2" $ do
            let expected = ZonedTime $ T.ZonedTime (T.LocalTime day1 timeOfDay2) offset2
            parseDateTime "1979-05-27T00:32:00-07:00" expected,
          testCase "Date 3" $ do
            let expected = ZonedTime $ T.ZonedTime (T.LocalTime day1 timeOfDay3) offset2
            parseDateTime "1979-05-27T00:32:00.999999-07:00" expected,
          testCase "Date 4" $ do
            let expected = ZonedTime $ T.ZonedTime (T.LocalTime day1 timeOfDay1) offset1
            parseDateTime "1979-05-27 07:32:00Z" expected
        ]

    parseLocalTimeTests =
      testGroup
        "Parse LocalTime"
        [ testCase "Date 1" $ do
            let expected = LocalTime $ T.LocalTime day1 timeOfDay1
            parseDateTime "1979-05-27T07:32:00" expected,
          testCase "Date 2" $ do
            let expected = LocalTime $ T.LocalTime day1 timeOfDay3
            parseDateTime "1979-05-27T00:32:00.999999" expected
        ]

    parseTimeOfDayTests =
      testGroup
        "Parse TimeOfDay"
        [ testCase "Date 1" $ do
            let expected = TimeOfDay timeOfDay1
            parseDateTime "07:32:00" expected,
          testCase "Date 2" $ do
            let expected = TimeOfDay timeOfDay3
            parseDateTime "00:32:00.999999" expected
        ]

    parseDayTests =
      testGroup
        "Parse Day"
        [ testCase "Date 1" $ do
            let expected = Day day1
            parseDateTime "1979-05-27" expected
        ]
