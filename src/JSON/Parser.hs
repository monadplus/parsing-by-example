{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

-- | This module contains the logic for parsing JSON
module JSON.Parser (parse) where

import Control.Applicative (optional, (<|>))
import Control.Applicative.Combinators (sepBy)
import Data.Functor (void)
import qualified Data.HashMap.Strict as HashMap
import Data.Scientific
import Data.Text (Text)
import JSON.Class (JValue)
import qualified JSON.Class as JSON
import JSON.Lexer (Token)
import qualified JSON.Lexer as Lexer
import Text.Earley (Grammar, Prod, rule, (<?>))
import qualified Text.Earley as Earley

type MyProd r a =
  Prod
    r -- Similar to 's' in 'ST s a'
    Text -- expected
    Token -- input
    a -- result

render :: Token -> Text
render = \case
  Lexer.LeftBrace -> "{"
  Lexer.RightBrace -> "}"
  Lexer.LeftBracket -> "["
  Lexer.RightBracket -> "]"
  Lexer.Comma -> ","
  Lexer.Colon -> ":"
  Lexer.FalseLit -> "false"
  Lexer.TrueLit -> "true"
  Lexer.NullLit -> "null"
  Lexer.StringLit _ -> "a text literal"
  Lexer.RealLit _ -> "a real number literal"
  Lexer.IntLit _ -> "an integer literal"

parseReal :: MyProd r Scientific
parseReal = Earley.terminal matchNumber
  where
    matchNumber :: Token -> Maybe Scientific
    matchNumber (Lexer.RealLit n) = Just n
    matchNumber (Lexer.IntLit n) = Just (fromIntegral n)
    matchNumber _ = Nothing

parseText :: MyProd r Text
parseText = Earley.terminal matchString
  where
    matchString :: Token -> Maybe Text
    matchString (Lexer.StringLit t) = Just t
    matchString _ = Nothing

token :: Token -> MyProd r ()
token t = void $ Earley.token t <?> render t

parseBool :: MyProd r Bool
parseBool = do
  True <$ token Lexer.TrueLit
    <|> False <$ token Lexer.FalseLit

grammar :: Grammar r (MyProd r JValue)
grammar = mdo
  jNull <-
    rule $
      JSON.JNull <$ token Lexer.NullLit

  jBool <-
    rule $
      JSON.JBool <$> parseBool

  jString <-
    rule $
      JSON.JString <$> parseText

  jNumber <-
    rule $
      JSON.JNumber <$> parseReal

  jArray <-
    rule $
      JSON.JArray <$> do
        token Lexer.LeftBracket
        elements <- jValue `sepBy` token Lexer.Comma
        optional (token Lexer.Comma)
        token Lexer.RightBracket
        pure elements

  jObject <-
    rule $
      JSON.JObject <$> do
        token Lexer.LeftBrace
        kvs <-
          ( do
              key <- parseText
              token Lexer.Colon
              value <- jValue
              pure (key, value)
            )
            `sepBy` token Lexer.Comma
        token Lexer.RightBrace
        pure $ HashMap.fromList kvs

  jValue <-
    rule $
      jObject
        <|> jArray
        <|> jNumber
        <|> jString
        <|> jBool
        <|> jNull

  return jValue

parse :: String -> Text -> Either String JValue
parse sourceName inputText = do
  tokens <- Lexer.lex sourceName inputText
  case Earley.fullParses (Earley.parser grammar) tokens of
    ([], report) ->
      Left $ show report
    (result : _, _) ->
      Right result
