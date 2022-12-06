{-# LANGUAGE OverloadedStrings #-}

module Test.TOML.Common
  ( shouldParseDate,
    shouldParseInteger,
    shouldFailParsingInteger,
    day1,
    timeOfDay1,
    timeOfDay2,
    timeOfDay3,
    offset1,
    offset2,
  )
where

import Data.Text (Text)
import qualified Data.Time.Calendar.OrdinalDate as T
import qualified Data.Time.LocalTime as T
import TOML.Class (Value (..))
import TOML.Parser (dateTimeP, integerP)
import Test.Tasty.HUnit
import Text.Megaparsec (TraversableStream)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.Stream (VisualStream)

shouldParseDate :: Text -> Value -> Assertion
shouldParseDate s expected = Megaparsec.parse dateTimeP "" s `shouldParse` expected

shouldParseInteger :: Text -> Integer -> Assertion
shouldParseInteger s expected = Megaparsec.parse integerP "" s `shouldParse` expected

shouldFailParsingInteger :: Text -> Assertion
shouldFailParsingInteger s = Megaparsec.parse integerP "" `shouldFailOn` s

shouldParse ::
  ( ShowErrorComponent e,
    VisualStream s,
    TraversableStream s,
    Eq a,
    Show a
  ) =>
  Either (ParseErrorBundle s e) a ->
  a ->
  Assertion
parseResult `shouldParse` expected =
  case parseResult of
    Left errorBundle -> assertFailure $ Megaparsec.errorBundlePretty errorBundle
    Right result -> assertEqual "" expected result

shouldFailOn ::
  (HasCallStack, Show a) =>
  (s -> Either (ParseErrorBundle s e) a) ->
  s ->
  Assertion
p `shouldFailOn` s =
  case p s of
    Left _ -> return ()
    Right v ->
      assertFailure $
        "the parser is expected to fail, but it parsed: " ++ show v

day1 :: T.Day
day1 = T.YearDay 1979 147

timeOfDay1, timeOfDay2, timeOfDay3 :: T.TimeOfDay
timeOfDay1 = T.TimeOfDay 7 32 0
timeOfDay2 = T.TimeOfDay 0 32 0
timeOfDay3 = T.TimeOfDay 0 32 00.999999

offset1, offset2 :: T.TimeZone
offset1 = T.hoursToTimeZone 0
offset2 = T.hoursToTimeZone (-7)
