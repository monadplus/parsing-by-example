{-# LANGUAGE OverloadedStrings #-}

module Test.TOML.Date (tests) where

import qualified Data.Time.LocalTime as T
import TOML.Class (Value (..))
import Test.TOML.Common (day1, offset1, offset2, shouldParseDate, timeOfDay1, timeOfDay2, timeOfDay3)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Date"
    [ zonedTimeTests,
      localTimeTests,
      timeOfDayTests,
      dayTests
    ]

zonedTimeTests :: TestTree
zonedTimeTests =
  testGroup
    "ZonedTime"
    [ testCase "Parse 1979-05-27T07:32:00Z" $ do
        let expected = ZonedTime $ T.ZonedTime (T.LocalTime day1 timeOfDay1) offset1
        shouldParseDate "1979-05-27T07:32:00Z" expected,
      testCase "Parse 1979-05-27T00:32:00-07:00" $ do
        let expected = ZonedTime $ T.ZonedTime (T.LocalTime day1 timeOfDay2) offset2
        shouldParseDate "1979-05-27T00:32:00-07:00" expected,
      testCase "Parse 1979-05-27T00:32:00.999999-07:00" $ do
        let expected = ZonedTime $ T.ZonedTime (T.LocalTime day1 timeOfDay3) offset2
        shouldParseDate "1979-05-27T00:32:00.999999-07:00" expected,
      testCase "Parse 1979-05-27 07:32:00Z" $ do
        let expected = ZonedTime $ T.ZonedTime (T.LocalTime day1 timeOfDay1) offset1
        shouldParseDate "1979-05-27 07:32:00Z" expected
    ]

localTimeTests :: TestTree
localTimeTests =
  testGroup
    "LocalTime"
    [ testCase "Parse 1979-05-27T07:32:00" $ do
        let expected = LocalTime $ T.LocalTime day1 timeOfDay1
        shouldParseDate "1979-05-27T07:32:00" expected,
      testCase "Parse 1979-05-27T00:32:00.999999" $ do
        let expected = LocalTime $ T.LocalTime day1 timeOfDay3
        shouldParseDate "1979-05-27T00:32:00.999999" expected
    ]

timeOfDayTests :: TestTree
timeOfDayTests =
  testGroup
    "TimeOfDay"
    [ testCase "Parse 07:32:00" $ do
        let expected = TimeOfDay timeOfDay1
        shouldParseDate "07:32:00" expected,
      testCase "Parse 00:32:00.999999" $ do
        let expected = TimeOfDay timeOfDay3
        shouldParseDate "00:32:00.999999" expected
    ]

dayTests :: TestTree
dayTests =
  testGroup
    "Day"
    [ testCase "Parse 1979-05-27" $ do
        let expected = Day day1
        shouldParseDate "1979-05-27" expected
    ]
