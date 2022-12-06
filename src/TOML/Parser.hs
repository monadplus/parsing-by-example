{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TOML.Parser
  ( dateTimeP,
    integerP,
    floatP,
    stringP,
  )
where

import Control.Applicative
import qualified Control.Monad.Combinators as Combinators
import qualified Data.Char as Char
import Data.Fixed
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import qualified Data.Time as Time
import Data.Void (Void)
import TOML.Class (Value (..))
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Megaparsec.Parsec Void Text

space :: Parser ()
space =
  Lexer.space
    Megaparsec.Char.space1
    (Lexer.skipLineComment "#")
    empty

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

-- TODO:
boolP :: Parser Bool
boolP = undefined

dateTimeP :: Parser Value
dateTimeP = lexeme (parseTime <|> parseDateTime) <?> "time literal"
  where
    -- TimeOfDay
    parseTime :: Parser Value
    parseTime = TimeOfDay <$> Megaparsec.try parseTimeOfDay

    -- ZonedTime, LocalTime, Day
    parseDateTime :: Parser Value
    parseDateTime = do
      day <- parseDay
      mTimeOfDay <- Combinators.optional $ Megaparsec.try (("T" <|> " ") *> parseTimeOfDay)
      case mTimeOfDay of
        Nothing -> pure $ Day day
        Just timeOfDay -> do
          mTimeZone <- Combinators.optional parseTimeZone
          let localTime = Time.LocalTime day timeOfDay
          pure $ case mTimeZone of
            Nothing -> LocalTime localTime
            Just timeZone -> ZonedTime $ Time.ZonedTime localTime timeZone

    parseTimeZone :: Parser Time.TimeZone
    parseTimeZone = Time.minutesToTimeZone <$> ((0 <$ "Z") <|> offset) <?> "time zone offset"
      where
        offset = do
          sign <- ("+" <|> "-") <?> "sign"
          hh <- int2DigitsP
          _ <- ":"
          mm <- int2DigitsP
          let signFun = if sign == "+" then id else negate
          pure $ signFun (hh * 60 + mm)

    parseTimeOfDay :: Parser Time.TimeOfDay
    parseTimeOfDay = do
      hh <- int2DigitsP
      _ <- ":"
      mm <- int2DigitsP
      _ <- ":"
      ss <- picoP
      case Time.makeTimeOfDayValid hh mm ss of
        Just time -> pure time
        Nothing ->
          fail $
            "Invalid time of day: " <> show hh <> ":" <> show mm <> ":" <> show ss

    parseDay :: Parser Time.Day
    parseDay = do
      year <- parseYear
      _ <- "-"
      month <- int2DigitsP
      _ <- "-"
      day <- int2DigitsP
      case Time.fromGregorianValid year month day of
        Just date -> pure date
        Nothing ->
          fail $
            "Invalid day: " <> show year <> "-" <> show month <> "-" <> show day

    parseYear :: Parser Integer
    parseYear = read <$> Combinators.count 4 Megaparsec.Char.digitChar

    int2DigitsP :: Parser Int
    int2DigitsP = read <$> Combinators.count 2 Megaparsec.Char.digitChar

    picoP :: Parser Pico
    picoP = do
      int <- Combinators.count 2 Megaparsec.Char.digitChar
      frac <- optional $ "." *> (take 12 <$> Combinators.some Megaparsec.Char.digitChar)
      pure $ read $ case frac of
        Nothing -> int
        Just frac' -> int ++ "." ++ frac'

-- TODO: review
stringP :: Parser Text
stringP = lexeme $ do
  "\""

  let isText c =
        ('\x20' <= c && c <= '\x21')
          || ('\x23' <= c && c <= '\x5b')
          || ('\x5d' <= c && c <= '\x10FFFF')

  let unescaped =
        Megaparsec.takeWhile1P
          (Just "text char")
          isText

  let unicodeEscape = do
        "\\u"
        codepoint <- Combinators.count 4 Megaparsec.Char.hexDigitChar
        case Read.hexadecimal (Text.pack codepoint) of
          Right (n, "") -> do
            return (Text.singleton (Char.chr n))
          _ -> do
            fail "invalid unicode"

  let escaped =
        Combinators.choice
          [ "\"" <$ "\\\"",
            "\\" <$ "\\\\",
            "/" <$ "\\/",
            "\b" <$ "\\b",
            "\f" <$ "\\f",
            "\n" <$ "\\n",
            "\r" <$ "\\r",
            "\t" <$ "\\t",
            unicodeEscape
          ]
          <?> "escape sequence"

  texts <- many (unescaped <|> escaped)

  "\""

  return $ Text.concat texts

floatP :: Parser Double
floatP =
  lexeme $
    Lexer.signed
      space
      ( Combinators.choice
          [ infP <?> "inf",
            nanP <?> "nan",
            doubleP <?> "float"
          ]
      )
  where
    infP, nanP, doubleP :: Parser Double
    infP = 1 / 0 <$ symbol "inf"
    nanP = 0 / 0 <$ symbol "nan"
    doubleP =
      maybe (fail "Invalid float") pure
        . Megaparsec.parseMaybe @Void Lexer.float
        =<< mconcat [digitsP, fracP <|> expP]
      where
        digitsP :: Parser Text
        digitsP = Text.pack . concat <$> Combinators.sepBy1 (Combinators.some Megaparsec.Char.digitChar) (Megaparsec.Char.char '_')

        fracP :: Parser Text
        fracP = mconcat [".", digitsP, Combinators.option "" expP]

        expP :: Parser Text
        expP =
          mconcat
            [ "e" <|> "E",
              Combinators.option "+" ("+" <|> "-"),
              digitsP
            ]

integerP :: Parser Integer
integerP =
  lexeme
    ( Combinators.choice
        [ Megaparsec.try "0b" *> binaryP <?> "binary",
          Megaparsec.try "0x" *> hexadecimalP <?> "hexadecimal",
          Megaparsec.try "0o" *> octalP <?> "octal",
          decimalP <?> "decimal"
        ]
    )
    <?> "integer"
  where
    numberP :: Parser Integer -> Parser Char -> Parser Integer
    numberP parseNumber parseDigit =
      failOnNothing . string2Number =<< digitsP
      where
        digitsP :: Parser Text
        digitsP = Text.pack . mconcat <$> Combinators.sepBy1 (Combinators.some parseDigit) "_"

        string2Number :: Text -> Maybe Integer
        string2Number = Megaparsec.parseMaybe @Void parseNumber

        failOnNothing :: Maybe a -> Parser a
        failOnNothing = maybe (fail "Invalid binary number") pure

    binaryP :: Parser Integer
    binaryP = numberP Lexer.binary Megaparsec.Char.binDigitChar

    hexadecimalP :: Parser Integer
    hexadecimalP = numberP Lexer.hexadecimal Megaparsec.Char.hexDigitChar

    octalP :: Parser Integer
    octalP = numberP Lexer.octal Megaparsec.Char.octDigitChar

    decimalP :: Parser Integer
    decimalP = do
      leadingZero <- Megaparsec.observing (Megaparsec.try leadingZeroP)
      case leadingZero of
        Left _ -> Lexer.signed space decP
        Right _ -> fail "Leading zeros are not allowed"
      where
        decP :: Parser Integer
        decP = numberP Lexer.decimal Megaparsec.Char.digitChar

        signP = Combinators.optional (Megaparsec.Char.char '+' <|> Megaparsec.Char.char '-')

        leadingZeroP = signP >> Megaparsec.Char.char '0' >> Combinators.some Megaparsec.Char.digitChar

-- Tip: parse the first value, the rest should use the same parser
arrayP :: Parser [Value]
arrayP = undefined

valueP :: Parser Value
valueP = undefined

-- TODO: 
-- tomlToken:
--  - Parse table name
--  - Parse table array name
--  - Parse key value
--     - KeyValue should parse key = inline table/inline array table/value
-- Once you have the toml tokens, you should type check it and transform it to a TOML object (~ hashmap)

-- {-# INLINEABLE parse #-}
-- parse :: String -> Text -> Either String [Token]
-- parse sourceName inputText =
--   case Megaparsec.parse parseTokens sourceName inputText of
--     Left ParseErrorBundle {..} -> do
--       let bundleError :| _ = bundleErrors
--       Left (Megaparsec.parseErrorPretty bundleError)
--     Right tokens -> do
--       return tokens
