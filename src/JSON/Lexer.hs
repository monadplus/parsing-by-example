{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module JSON.Lexer
  ( Token (..),
    lex,
  )
where

import Control.Applicative (empty, (<|>))
import Control.Monad.Combinators (many, manyTill)
import qualified Control.Monad.Combinators as Combinators
import Data.Char (isControl)
import qualified Data.Char as Char
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import Data.Void (Void)
import Text.Megaparsec (ParseError, (<?>))
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Error (ParseErrorBundle (ParseErrorBundle))
import Prelude hiding (lex)

type Parser = Megaparsec.Parsec Void Text

space :: Parser ()
space = Lexer.space Megaparsec.Char.space1 empty empty

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

data Token
  = LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Comma
  | Colon
  | FalseLit
  | TrueLit
  | NullLit
  | StringLit Text
  | RealLit Scientific
  | IntLit Int
  deriving stock (Eq, Show)

-- https://datatracker.ietf.org/doc/html/rfc7159#section-7
string :: Parser Token
string = lexeme $ do
  "\""

  let unescaped =
        Megaparsec.takeWhile1P
          (Just "text char")
          (\c -> not (c == '\"' || c == '\\' || isControl c))

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

number :: Parser Token
number = do
  scientific <- lexeme Lexer.scientific
  case Scientific.toBoundedInteger scientific of
    Just int -> return (IntLit int)
    Nothing -> return (RealLit scientific)

parseToken :: Parser Token
parseToken =
  Combinators.choice
    [ Combinators.choice
        [ FalseLit <$ symbol "false",
          TrueLit <$ symbol "true",
          NullLit <$ symbol "null"
        ],
      LeftBrace <$ symbol "{",
      RightBrace <$ symbol "}",
      LeftBracket <$ symbol "[",
      RightBracket <$ symbol "]",
      Comma <$ symbol ",",
      Colon <$ symbol ":",
      string,
      number
    ]

parseTokens :: Parser [Token]
parseTokens = do
  space
  manyTill parseToken Megaparsec.eof

lex :: String -> Text -> Either (ParseError Text Void) [Token]
lex sourceName inputText =
  case Megaparsec.parse parseTokens sourceName inputText of
    Left ParseErrorBundle {..} -> do
      let bundleError :| _ = bundleErrors
      Left bundleError
    Right tokens -> do
      return tokens
