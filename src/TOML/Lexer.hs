{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TOML.Lexer
  ( Token (..),
    TimeLit (..),
    lex,
  )
where

import Control.Applicative
import qualified Control.Monad.Combinators as Combinators
import qualified Data.Char as Char
import Data.Fixed
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import qualified Data.Time as Time
import Data.Void (Void)
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Error (ParseErrorBundle (..))
import Prelude hiding (lex)

type Parser = Megaparsec.Parsec Void Text

{-# INLINEABLE space #-}
space :: Parser ()
space =
  Lexer.space
    Megaparsec.Char.space1
    (Lexer.skipLineComment "#")
    empty

{-# INLINEABLE symbol #-}
symbol :: Text -> Parser Text
symbol = Lexer.symbol space

{-# INLINEABLE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

data TimeLit
  = ZonedTime Time.ZonedTime
  | LocalTime Time.LocalTime
  | TimeOfDay Time.TimeOfDay
  | Day Time.Day
  deriving stock (Show)

instance Eq TimeLit where
  ZonedTime z1 == ZonedTime z2 =
    Time.zonedTimeToUTC z1 == Time.zonedTimeToUTC z2
  LocalTime lt1 == LocalTime lt2 = lt1 == lt2
  TimeOfDay tod1 == TimeOfDay tod2 = tod1 == tod2
  Day d1 == Day d2 = d1 == d2
  _ == _ = False

data Token
  = LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | FalseLit
  | TrueLit
  | StringLit Text
  | RealLit Scientific
  | IntLit Int
  | TimeLit TimeLit
  deriving stock (Eq, Show)

{-# INLINEABLE parseTimeLit #-}
parseTimeLit :: Parser Token
parseTimeLit = TimeLit <$> lexeme (parseTime <|> parseDateTime) <?> "time literal"
  where
    -- TimeOfDay
    parseTime :: Parser TimeLit
    parseTime = TimeOfDay <$> Megaparsec.try parseTimeOfDay

    -- ZonedTime, LocalTime, Day
    parseDateTime :: Parser TimeLit
    parseDateTime = do
      day <- parseDay
      mTimeOfDay <- optional $ Megaparsec.try (("T" <|> " ") *> parseTimeOfDay)
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
          hh <- parse2DigitsInt
          _ <- ":"
          mm <- parse2DigitsInt
          let signFun = if sign == "+" then id else negate
          pure $ signFun (hh * 60 + mm)

    parseTimeOfDay :: Parser Time.TimeOfDay
    parseTimeOfDay = do
      hh <- parse2DigitsInt
      _ <- ":"
      mm <- parse2DigitsInt
      _ <- ":"
      ss <- parsePico
      case Time.makeTimeOfDayValid hh mm ss of
        Just time -> pure time
        Nothing ->
          fail $
            "Invalid time of day: " <> show hh <> ":" <> show mm <> ":" <> show ss

    parseDay :: Parser Time.Day
    parseDay = do
      year <- parseYear
      _ <- "-"
      month <- parse2DigitsInt
      _ <- "-"
      day <- parse2DigitsInt
      case Time.fromGregorianValid year month day of
        Just date -> pure date
        Nothing ->
          fail $
            "Invalid day: " <> show year <> "-" <> show month <> "-" <> show day

    parseYear :: Parser Integer
    parseYear = read <$> Combinators.count 4 Megaparsec.Char.digitChar

parse2DigitsInt :: Parser Int
parse2DigitsInt = read <$> Combinators.count 2 Megaparsec.Char.digitChar

parsePico :: Parser Pico
parsePico = do
  int <- Combinators.count 2 Megaparsec.Char.digitChar
  frac <- Combinators.optional $ "." *> (take 12 <$> some Megaparsec.Char.digitChar)
  pure $ read $ case frac of
    Nothing -> int
    Just frac' -> int ++ "." ++ frac'

{-# INLINEABLE string #-}
string :: Parser Token
string = lexeme $ do
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

  return (StringLit (Text.concat texts))

-- TODO: separator _
{-# INLINEABLE number #-}
number :: Parser Token
number = do
  scientific <- Lexer.signed space (lexeme Lexer.scientific)
  case Scientific.toBoundedInteger scientific of
    Just int -> return (IntLit int)
    Nothing -> return (RealLit scientific)

{-# INLINEABLE parseToken #-}
parseToken :: Parser Token
parseToken =
  Combinators.choice
    [ Combinators.choice
        [ FalseLit <$ symbol "false",
          TrueLit <$ symbol "true"
        ]
        <?> "literal",
      LeftBrace <$ symbol "{",
      RightBrace <$ symbol "}",
      LeftBracket <$ symbol "[",
      RightBracket <$ symbol "]",
      string,
      parseTimeLit, -- Try before number
      number
    ]

{-# INLINEABLE parseTokens #-}
parseTokens :: Parser [Token]
parseTokens = do
  space
  Combinators.manyTill parseToken Megaparsec.eof

{-# INLINEABLE lex #-}
lex :: String -> Text -> Either String [Token]
lex sourceName inputText =
  case Megaparsec.parse parseTokens sourceName inputText of
    Left ParseErrorBundle {..} -> do
      let bundleError :| _ = bundleErrors
      Left (Megaparsec.parseErrorPretty bundleError)
    Right tokens -> do
      return tokens
