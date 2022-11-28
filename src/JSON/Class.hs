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
import JSON.Pretty (Pretty (..))
import qualified JSON.Pretty as Pretty
import Numeric (showHex)
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as Pretty
import Prettyprinter.Render.Terminal (AnsiStyle)

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

instance Pretty JValue where
  pretty JNull =
    Pretty.jsonLiteral "null"
  pretty (JBool True) =
    Pretty.jsonLiteral "true"
  pretty (JBool False) =
    Pretty.jsonLiteral "false"
  pretty (JString text) =
    Pretty.jsonString (pretty $ showJSONString text)
  pretty (JNumber scientific) =
    Pretty.jsonNumber (pretty scientific)
  pretty (JArray elements) =
    prettyJArray elements
  pretty (JObject hashmap) =
    prettyJObject (HashMap.toList hashmap)

prettyJArray :: [JValue] -> Doc AnsiStyle
prettyJArray [] = "[ ]"
prettyJArray (element : elements) =
  Pretty.align $ Pretty.flatAlt long short
  where
    long =
      "[ "
        <> ( pretty element
               <> foldMap (\e -> Pretty.hardline <> "," <+> pretty e) elements
           )
        <> Pretty.hardline
        <> "]"
    short =
      Pretty.list (pretty <$> elements)

prettyJObject :: [(Text, JValue)] -> Doc AnsiStyle
prettyJObject [] = "{ }"
prettyJObject kvs =
  Pretty.align $ Pretty.group (Pretty.flatAlt long short)
  where
    short = Pretty.encloseSep "{" "}" "," (prettyShortKV <$> kvs)

    long = Pretty.encloseSep "{" "}" "," (prettyLongKV <$> kvs)

    prettyShortKV (key, value) =
      Pretty.jsonKey (pretty $ showJSONString key)
        <+> ":"
        <+> pretty value

    prettyLongKV (key, value) =
      Pretty.jsonKey (pretty $ showJSONString key)
        <+> ":"
          <> Pretty.group (Pretty.flatAlt (Pretty.hardline <> "    ") " ")
          <> pretty value
