{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TOML.Parser
  ( arrayP,
    boolP,
    dateTimeP,
    floatP,
    integerP,
    stringP,
    valueP,
    keyP,
    tableHeaderP,
    tableHeaderArrayP,
    keyValueP,
  )
where

import Control.Applicative
import qualified Control.Monad.Combinators as Combinators
import qualified Control.Monad.Combinators.NonEmpty as Combinators.NonEmpty
import qualified Data.Char as Char
import Data.Fixed
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import qualified Data.Time as Time
import Data.Void (Void)
import TOML.Class (Key (..), KeyComponent (..), TomlNode (..), Value (..))
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

boolP :: Parser Value
boolP =
  Bool
    <$> lexeme
      ( Combinators.choice
          [ True <$ symbol "true",
            False <$ symbol "false"
          ]
      )
    <?> "boolean"

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

stringP :: Parser Value
stringP =
  String
    <$> Combinators.choice
      [ multiLineBasicStringP <?> "multi-line basic string",
        basicStringP <?> "basic string",
        multiLineLiteralStringP <?> "multi-line literal string",
        literalStringP <?> "literal string"
      ]
    <?> "string"

-- Includes whitespaces but not newlines
nonControlCharP :: Parser Text
nonControlCharP =
  Text.singleton <$> (Megaparsec.satisfy (not . Char.isControl) <?> "non-control char")

escapedP :: Parser Text
escapedP = Text.singleton <$> escapedP'
  where
    escapedP' :: Parser Char
    escapedP' =
      Megaparsec.Char.char '\\'
        *> Combinators.choice
          [ Megaparsec.Char.char '"',
            Megaparsec.Char.char '\\',
            Megaparsec.Char.char '/',
            Megaparsec.Char.char 'b' $> '\b',
            Megaparsec.Char.char 'f' $> '\f',
            Megaparsec.Char.char 'n' $> '\n',
            Megaparsec.Char.char 'r' $> '\r',
            Megaparsec.Char.char 't' $> '\t',
            Megaparsec.Char.char 'u' *> unicodeP 4,
            Megaparsec.Char.char 'U' *> unicodeP 8
          ]
        <?> "escape sequence"
    unicodeP :: Int -> Parser Char
    unicodeP = \digits -> do
      codepoint <- Combinators.count digits Megaparsec.Char.hexDigitChar
      case Read.hexadecimal (Text.pack codepoint) of
        Right (n, "") -> do
          return $ Char.chr n
        _ -> do
          fail "invalid unicode"

basicStringP :: Parser Text
basicStringP = lexeme $ mconcat <$> (dQuoteP *> Combinators.manyTill charP dQuoteP)
  where
    dQuoteP = Megaparsec.Char.char '"'

    charP = escapedP <|> nonControlCharP

literalStringP :: Parser Text
literalStringP = lexeme $ Text.pack <$> (quote *> Combinators.manyTill charP quote)
  where
    quote = Megaparsec.Char.char '\''
    charP = Megaparsec.satisfy (\c -> c /= '\n' && c /= '\r')

-- FIXME:
-- There's a subtle bug that happens when the closing delimiter is prefixed with
-- another closing delimiter component e.g. """Foo"""" should be parsed as 'Foo"' but
-- is actually parsed as 'Foo'.
multiLineStringP :: Parser Text -> Parser Text -> Parser Text
multiLineStringP delimiterP charP =
  lexeme $
    mconcat
      <$> ( delimiterP
              *> Combinators.optional Megaparsec.Char.eol
              *> Combinators.manyTill charP delimiterP
          )

multiLineBasicStringP :: Parser Text
multiLineBasicStringP = multiLineStringP delimiterP charP
  where
    delimiterP = symbol "\"\"\""

    backslashEndP = Megaparsec.try (Megaparsec.Char.char '\\' *> Megaparsec.Char.eol *> Megaparsec.Char.space) $> Text.empty

    charP = backslashEndP <|> escapedP <|> nonControlCharP <|> Megaparsec.Char.eol

multiLineLiteralStringP :: Parser Text
multiLineLiteralStringP = multiLineStringP delimiterP charP
  where
    delimiterP = symbol "'''"

    tab = Text.singleton <$> Megaparsec.Char.tab

    charP = nonControlCharP <|> Megaparsec.Char.eol <|> tab

floatP :: Parser Value
floatP =
  Float
    <$> lexeme
      ( Lexer.signed
          space
          ( Combinators.choice
              [ infP <?> "inf",
                nanP <?> "nan",
                doubleP <?> "float"
              ]
          )
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

integerP :: Parser Value
integerP =
  Integer
    <$> lexeme
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

arrayP :: Parser Value
arrayP =
  Array
    <$> lexeme
      ( Combinators.between ("[" *> space) "]" $
          Combinators.sepEndBy valueP (symbol "," *> space)
      )

valueP :: Parser Value
valueP =
  Combinators.choice
    [ Megaparsec.try arrayP,
      Megaparsec.try boolP,
      Megaparsec.try stringP,
      Megaparsec.try dateTimeP,
      Megaparsec.try floatP,
      Megaparsec.try integerP
    ]

keyP :: Parser Key
keyP = Key <$> Combinators.NonEmpty.sepBy1 keyComponentP (Megaparsec.Char.char '.' *> space) <?> "key"
  where
    bareKeyP = lexeme $ Megaparsec.takeWhile1P Nothing (\c -> Char.isAlphaNum c || c == '-' || c == '_')

    keyComponentP = KeyComponent <$> (bareKeyP <|> basicStringP <|> literalStringP)

tableHeaderP :: Parser Key
tableHeaderP = Combinators.between (symbol "[") (symbol "]") keyP

tableHeaderArrayP :: Parser Key
tableHeaderArrayP = Combinators.between (symbol "[[") (symbol "]]") keyP

keyValueP :: Parser TomlNode
keyValueP = do
  key <- keyP <* symbol "="
  KeyValue key <$> valueP

{- TODO:
\* Parse table
\* Parse inline table
\* Parse table array
-}
