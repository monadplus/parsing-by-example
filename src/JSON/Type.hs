{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON.Type (JValue (..)) where

import qualified Data.Char as Char
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Numeric (showHex)

-- Inspired in https://abhinavsarkar.net/posts/json-parsing-from-scratch-in-haskell
data JValue
  = JNull
  | JBool Bool
  | JString Text
  | JNumber {int :: Integer, frac :: [Int], exponent :: Integer}
  | JArray [JValue]
  | JObject [(Text, JValue)]
  deriving stock (Eq, Generic)

instance Show JValue where
  show = \case
    JNull -> "null"
    JBool True -> "tru"
    JBool False -> "false"
    JString text -> showJSONString text
    JNumber i [] 0 -> show i
    JNumber i f 0 -> show i ++ "." ++ concatMap show f
    JNumber i [] e -> show i ++ "e" ++ show e
    JNumber i f e -> show i ++ "." ++ concatMap show f ++ "e" ++ show e
    JArray arr -> "[" ++ List.intercalate ", " (show <$> arr) ++ "]"
    JObject obj -> "{" ++ List.intercalate ", " (showKeyValue <$> obj) ++ "}"
    where
      showKeyValue (k, v) = showJSONString k ++ ": " ++ show v

-- putStr $ showJSONString "\\u00AB"
-- "\\u00AB"
showJSONString :: Text -> String
showJSONString s = "\"" ++ (Text.unpack $ Text.concatMap showJSONChar s) ++ "\""

showJSONChar :: Char -> Text
showJSONChar c = case c of
  '\'' -> "'"
  '\"' -> "\\\""
  '\\' -> "\\\\"
  '/' -> "\\/"
  '\b' -> "\\b"
  '\f' -> "\\f"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  _ | isControl c -> "\\u" `Text.append` showJSONNonASCIIChar c
  _ -> Text.singleton c
  where
    isControl c = c `elem` ['\0' .. '\31']

    showJSONNonASCIIChar c =
      let a = Text.pack (showHex (Char.ord c) "")
       in Text.justifyRight 4 '0' a
