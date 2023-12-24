{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.TOML.Common
  ( shouldParseDate,
    shouldParseInteger,
    shouldFailParsingInteger,
    shouldParseFloat,
    shouldParseArray,
    shouldParseKey,
    shouldFailParsingFloat,
    shouldFailParsingToml,
    shouldParseBool,
    shouldFailParsingBool,
    shouldParseString,
    shouldFailParsingString,
    shouldFailParsingArray,
    shouldFailParsingKey,
    shouldFailParsingTableHeader,
    shouldFailParsingTableHeaderArray,
    shouldParseTableHeader,
    shouldParseTableHeaderArray,
    shouldParseToml,
    day1,
    timeOfDay1,
    timeOfDay2,
    timeOfDay3,
    offset1,
    offset2,
    zonedTime1,
    localTime1,
    quote,
    tripleQuote,
    dQuote,
    tripleDQuote,
    int1,
    int2,
    int3,
    float1,
    float2,
    float3,
    istring,
    iinteger,
    ifloat,
  )
where

import Data.Text (Text)
import qualified Data.Time.Calendar.OrdinalDate as T
import qualified Data.Time.LocalTime as T
import TOML.Class (IValue (..), Key (..), TomlAst, Value (..))
import qualified TOML.Parser
import Test.Tasty.HUnit
import Text.Megaparsec (TraversableStream)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.Stream (VisualStream)

shouldParseDate :: Text -> Value -> Assertion
shouldParseDate s expected = Megaparsec.parse TOML.Parser.dateTimeP "" s `shouldParse` expected

shouldParseInteger :: Text -> Integer -> Assertion
shouldParseInteger s expected = Megaparsec.parse TOML.Parser.integerP "" s `shouldParse` Integer expected

shouldParseFloat :: Text -> Double -> Assertion
shouldParseFloat s expected = do
  let d = (\(Float x) -> x) <$> Megaparsec.parse TOML.Parser.floatP "" s
  fmap DoubleNaN d `shouldParse` DoubleNaN expected

shouldParseBool :: Text -> Bool -> Assertion
shouldParseBool s expected = Megaparsec.parse TOML.Parser.boolP "" s `shouldParse` Bool expected

shouldParseString :: Text -> Text -> Assertion
shouldParseString s expected = Megaparsec.parse TOML.Parser.stringP "" s `shouldParse` String expected

shouldParseArray :: Text -> [Value] -> Assertion
shouldParseArray s expected = Megaparsec.parse TOML.Parser.arrayP "" s `shouldParse` Array expected

shouldParseKey :: Text -> Key -> Assertion
shouldParseKey s expected = Megaparsec.parse TOML.Parser.keyP "" s `shouldParse` expected

shouldParseTableHeader :: Text -> Key -> Assertion
shouldParseTableHeader s expected = Megaparsec.parse TOML.Parser.tableHeaderP "" s `shouldParse` expected

shouldParseTableHeaderArray :: Text -> Key -> Assertion
shouldParseTableHeaderArray s expected = Megaparsec.parse TOML.Parser.tableHeaderArrayP "" s `shouldParse` expected

shouldParseToml :: Text -> [TomlAst] -> Assertion
shouldParseToml s expected = Megaparsec.parse TOML.Parser.tomlP "" s `shouldParse` expected

shouldFailParsingInteger,
  shouldFailParsingFloat,
  shouldFailParsingBool,
  shouldFailParsingString,
  shouldFailParsingArray,
  shouldFailParsingKey,
  shouldFailParsingTableHeader,
  shouldFailParsingTableHeaderArray,
  shouldFailParsingToml ::
    Text -> Assertion
shouldFailParsingString s = Megaparsec.parse (TOML.Parser.stringP *> Megaparsec.eof) "" `shouldFailOn` s
shouldFailParsingInteger s = Megaparsec.parse (TOML.Parser.integerP *> Megaparsec.eof) "" `shouldFailOn` s
shouldFailParsingFloat s = Megaparsec.parse (TOML.Parser.floatP *> Megaparsec.eof) "" `shouldFailOn` s
shouldFailParsingBool s = Megaparsec.parse (TOML.Parser.boolP *> Megaparsec.eof) "" `shouldFailOn` s
shouldFailParsingArray s = Megaparsec.parse (TOML.Parser.arrayP *> Megaparsec.eof) "" `shouldFailOn` s
shouldFailParsingKey s = Megaparsec.parse (TOML.Parser.keyP *> Megaparsec.eof) "" `shouldFailOn` s
shouldFailParsingTableHeader s = Megaparsec.parse (TOML.Parser.tableHeaderP *> Megaparsec.eof) "" `shouldFailOn` s
shouldFailParsingTableHeaderArray s = Megaparsec.parse (TOML.Parser.tableHeaderArrayP *> Megaparsec.eof) "" `shouldFailOn` s
shouldFailParsingToml s = Megaparsec.parse (TOML.Parser.tomlP *> Megaparsec.eof) "" `shouldFailOn` s

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

int1, int2, int3 :: Value
int1 = Integer 1
int2 = Integer 2
int3 = Integer 3

float1, float2, float3 :: Value
float1 = Float 1.0
float2 = Float 2.0
float3 = Float 3.0

day1 :: T.Day
day1 = T.YearDay 1979 147

timeOfDay1, timeOfDay2, timeOfDay3 :: T.TimeOfDay
timeOfDay1 = T.TimeOfDay 7 32 0
timeOfDay2 = T.TimeOfDay 0 32 0
timeOfDay3 = T.TimeOfDay 0 32 00.999999

offset1, offset2 :: T.TimeZone
offset1 = T.hoursToTimeZone 0
offset2 = T.hoursToTimeZone (-7)

zonedTime1 :: Value
zonedTime1 = ZonedTime $ T.ZonedTime (T.LocalTime day1 timeOfDay1) offset1

localTime1 :: Value
localTime1 = LocalTime $ T.LocalTime day1 timeOfDay1

newtype DoubleNaN = DoubleNaN Double
  deriving newtype (Show)

instance Eq DoubleNaN where
  DoubleNaN f1 == DoubleNaN f2
    | isNaN f1 && isNaN f2 = True
    | otherwise = f1 == f2

quote :: Text -> Text
quote s = "'" <> s <> "'"

tripleQuote :: Text -> Text
tripleQuote s = "'''" <> s <> "'''"

dQuote :: Text -> Text
dQuote s = "\"" <> s <> "\""

tripleDQuote :: Text -> Text
tripleDQuote s = "\"\"\"" <> s <> "\"\"\""

istring :: Text -> IValue
istring = IValue . String

iinteger :: Integer -> IValue
iinteger = IValue . Integer

ifloat :: Double -> IValue
ifloat = IValue . Float
