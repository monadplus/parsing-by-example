{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON.Class (JValue (..), showJSONString) where

import qualified Data.Char as Char
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Numeric (showHex)

data JValue
  = JNull
  | JBool Bool
  | JString Text
  | JNumber Scientific
  | JArray [JValue]
  | JObject (HashMap Text JValue)
  deriving stock (Eq, Generic)

instance Show JValue where
  show = \case
    JNull -> "null"
    JBool True -> "true"
    JBool False -> "false"
    JString text -> showJSONString text
    JNumber scientific -> show scientific
    JArray arr -> "[" ++ List.intercalate ", " (show <$> arr) ++ "]"
    JObject obj -> "{" ++ List.intercalate ", " (showKeyValue <$> HashMap.toList obj) ++ "}"
    where
      showKeyValue (k, v) = showJSONString k ++ ": " ++ show v

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
    isControl c' = c' `elem` ['\0' .. '\31']

    showJSONNonASCIIChar c' =
      let a = Text.pack (showHex (Char.ord c') "")
       in Text.justifyRight 4 '0' a
