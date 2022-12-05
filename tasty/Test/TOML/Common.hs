{-# LANGUAGE OverloadedStrings #-}

module Test.TOML.Common
  ( parseDateTime,
    day1,
    timeOfDay1,
    timeOfDay2,
    timeOfDay3,
    offset1,
    offset2
  )
where

import Data.Text (Text)
import qualified Data.Time.Calendar.OrdinalDate as T
import qualified Data.Time.LocalTime as T
import TOML.Class (Value (..))
import TOML.Parser (dateTimeP)
import Test.Tasty.HUnit
import Text.Megaparsec (Parsec, TraversableStream)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.Stream (VisualStream)

parseDateTime :: Text -> Value -> Assertion
parseDateTime = shouldParse dateTimeP

shouldParse ::
  ( ShowErrorComponent e,
    VisualStream s,
    TraversableStream s,
    Eq a,
    Show a
  ) =>
  Parsec e s a ->
  s ->
  a ->
  Assertion
shouldParse p given expected =
  case Megaparsec.parse p "" given of
    Left errorBundle -> assertFailure $ Megaparsec.errorBundlePretty errorBundle
    Right result -> assertEqual "" expected result

day1 :: T.Day
day1 = T.YearDay 1979 147

timeOfDay1, timeOfDay2, timeOfDay3 :: T.TimeOfDay
timeOfDay1 = T.TimeOfDay 7 32 0
timeOfDay2 = T.TimeOfDay 0 32 0
timeOfDay3 = T.TimeOfDay 0 32 00.999999

offset1, offset2 :: T.TimeZone
offset1 = T.hoursToTimeZone 0
offset2 = T.hoursToTimeZone (-7)
