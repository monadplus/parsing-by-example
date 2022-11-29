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
import Pretty (Pretty (..))
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..))
import qualified Prettyprinter.Render.Terminal as PP.Terminal

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
showJSONString s = "\"" ++ Text.unpack (Text.concatMap showJSONChar s) ++ "\""

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
    jsonLiteral "null"
  pretty (JBool True) =
    jsonLiteral "true"
  pretty (JBool False) =
    jsonLiteral "false"
  pretty (JString text) =
    jsonString (pretty $ showJSONString text)
  pretty (JNumber scientific) =
    jsonNumber (pretty scientific)
  pretty (JArray elements) =
    prettyJArray elements
  pretty (JObject hashmap) =
    prettyJObject (HashMap.toList hashmap)

-- | JSON literals e.g. null, true, false
jsonLiteral :: Doc AnsiStyle -> Doc AnsiStyle
jsonLiteral = PP.annotate (PP.Terminal.colorDull Red)

jsonKey :: Doc AnsiStyle -> Doc AnsiStyle
jsonKey = PP.annotate (PP.Terminal.color Green)

jsonString :: Doc AnsiStyle -> Doc AnsiStyle
jsonString = PP.annotate (PP.Terminal.colorDull Yellow)

jsonNumber :: Doc AnsiStyle -> Doc AnsiStyle
jsonNumber = PP.annotate (PP.Terminal.colorDull Cyan)

prettyJArray :: [JValue] -> Doc AnsiStyle
prettyJArray [] = "[ ]"
prettyJArray (element : elements) =
  PP.align $ PP.flatAlt long short
  where
    long =
      "[ "
        <> ( pretty element
               <> foldMap (\e -> PP.hardline <> "," <+> pretty e) elements
           )
        <> PP.hardline
        <> "]"
    short =
      PP.list (pretty <$> elements)

prettyJObject :: [(Text, JValue)] -> Doc AnsiStyle
prettyJObject [] = "{ }"
prettyJObject kvs =
  PP.align $ PP.group (PP.flatAlt long short)
  where
    short = PP.encloseSep "{" "}" "," (prettyShortKV <$> kvs)

    long = PP.encloseSep "{" "}" "," (prettyLongKV <$> kvs)

    prettyShortKV (key, value) =
      jsonKey (pretty $ showJSONString key)
        <+> ":"
        <+> pretty value

    prettyLongKV (key, value) =
      jsonKey (pretty $ showJSONString key)
        <+> ":"
          <> PP.group (PP.flatAlt (PP.hardline <> "    ") " ")
          <> pretty value
