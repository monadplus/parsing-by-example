{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.TOML.Common
  ( shouldParseDate,
    shouldParseInteger,
    shouldFailParsingInteger,
    shouldParseFloat,
    shouldFailParsingFloat,
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
import qualified TOML.Parser
import Test.Tasty.HUnit
import Text.Megaparsec (TraversableStream)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.Stream (VisualStream)

shouldParseDate :: Text -> Value -> Assertion
shouldParseDate s expected = Megaparsec.parse TOML.Parser.dateTimeP "" s `shouldParse` expected

shouldParseInteger :: Text -> Integer -> Assertion
shouldParseInteger s expected = Megaparsec.parse TOML.Parser.integerP "" s `shouldParse` expected

shouldFailParsingInteger :: Text -> Assertion
shouldFailParsingInteger s = Megaparsec.parse TOML.Parser.integerP "" `shouldFailOn` s

shouldParseFloat :: Text -> Double -> Assertion
shouldParseFloat s expected = do
  let d = Megaparsec.parse TOML.Parser.floatP "" s
  fmap DoubleNaN d `shouldParse` DoubleNaN expected

shouldFailParsingFloat :: Text -> Assertion
shouldFailParsingFloat s = Megaparsec.parse TOML.Parser.floatP "" `shouldFailOn` s

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

newtype DoubleNaN = DoubleNaN Double
  deriving newtype (Show)

instance Eq DoubleNaN where
  DoubleNaN f1 == DoubleNaN f2
    | isNaN f1 && isNaN f2 = True
    | otherwise = f1 == f2
