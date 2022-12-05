{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TOML.Parser
  ( dateTimeP,
  )
where

import Control.Applicative
import Control.Monad.Combinators (choice, count)
import qualified Data.Char as Char
import Data.Fixed
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import qualified Data.Time as Time
import Data.Void (Void)
import TOML.Class (Value (..))
import Text.Megaparsec (takeWhile1P, try, (<?>))
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char (digitChar, hexDigitChar, space1)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Prelude hiding (lex)

type Parser = Megaparsec.Parsec Void Text

space :: Parser ()
space =
  Lexer.space
    space1
    (Lexer.skipLineComment "#")
    empty

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

dateTimeP :: Parser Value
dateTimeP = lexeme (parseTime <|> parseDateTime) <?> "time literal"
  where
    -- TimeOfDay
    parseTime :: Parser Value
    parseTime = TimeOfDay <$> try parseTimeOfDay

    -- ZonedTime, LocalTime, Day
    parseDateTime :: Parser Value
    parseDateTime = do
      day <- parseDay
      mTimeOfDay <- optional $ try (("T" <|> " ") *> parseTimeOfDay)
      case mTimeOfDay of
        Nothing -> pure $ Day day
        Just timeOfDay -> do
          mTimeZone <- optional parseTimeZone
          let localTime = Time.LocalTime day timeOfDay
          pure $ case mTimeZone of
            Nothing -> LocalTime localTime
            Just timeZone -> ZonedTime $ Time.ZonedTime localTime timeZone

    parseTimeZone :: Parser Time.TimeZone
    parseTimeZone = Time.minutesToTimeZone <$> ((0 <$ "Z") <|> offset) <?> "time zone offset"
      where
        offset = do
          sign <- ("+" <|> "-") <?> "sign (+ or -)"
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
    parseYear = read <$> count 4 digitChar

    int2DigitsP :: Parser Int
    int2DigitsP = read <$> count 2 digitChar

    picoP :: Parser Pico
    picoP = do
      int <- count 2 digitChar
      frac <- optional $ "." *> (take 12 <$> some digitChar)
      pure $ read $ case frac of
        Nothing -> int
        Just frac' -> int ++ "." ++ frac'

stringP :: Parser Value
stringP = lexeme $ do
  "\""

  let isText c =
        ('\x20' <= c && c <= '\x21')
          || ('\x23' <= c && c <= '\x5b')
          || ('\x5d' <= c && c <= '\x10FFFF')

  let unescaped =
        takeWhile1P
          (Just "text char")
          isText

  let unicodeEscape = do
        "\\u"
        codepoint <- count 4 hexDigitChar
        case Read.hexadecimal (Text.pack codepoint) of
          Right (n, "") -> do
            return (Text.singleton (Char.chr n))
          _ -> do
            fail "invalid unicode"

  let escaped =
        choice
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

  return (Text (Text.concat texts))

-- TODO: separator _
number :: Parser Value
number = do
  scientific <- Lexer.signed space (lexeme Lexer.scientific)
  case Scientific.toBoundedInteger scientific of
    Just int -> return (Integer int)
    Nothing -> return (Double scientific)

-- parseToken :: Parser Token
-- parseToken =
--   choice
--     [ string,
--       TimeLit <$> parseTimeLit, -- Try before number
--       number
--     ]

-- parseToml :: Parser [Token]
-- parseToml = do
--   space
--   manyTill parseToken eof

-- {-# INLINEABLE parse #-}
-- parse :: String -> Text -> Either String [Token]
-- parse sourceName inputText =
--   case Megaparsec.parse parseTokens sourceName inputText of
--     Left ParseErrorBundle {..} -> do
--       let bundleError :| _ = bundleErrors
--       Left (Megaparsec.parseErrorPretty bundleError)
--     Right tokens -> do
--       return tokens
