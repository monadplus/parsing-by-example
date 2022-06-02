{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module JSON.Gen
  ( jNullGen,
    jBoolGen,
    jNumberGen,
    jStringGen,
    jArrayGen,
    jObjectGen,
    jValueGen,
    stringify,
  )
where

import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int32, Int8)
import Data.Scientific (scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import JSON.Class (JValue (..), showJSONString)

jNullGen :: Gen JValue
jNullGen = pure JNull

jBoolGen :: Gen JValue
jBoolGen = JBool <$> Gen.bool

integerGen :: Gen Integer
integerGen = Gen.integral (fromIntegral <$> Range.constantBounded @Int32)

smallIntGen :: Gen Int
smallIntGen = Gen.int (fromIntegral <$> Range.constantBounded @Int8)

jNumberGen :: Gen JValue
jNumberGen = JNumber <$> scientificGen
  where
    scientificGen = scientific <$> integerGen <*> exponentGen
    exponentGen = Gen.frequency [(7, pure 0), (3, smallIntGen)]

jsonStringGen :: Range Int -> Gen Text
jsonStringGen listSize =
  Text.concat
    <$> ( Gen.list listSize $
            Gen.frequency
              [ (9, Text.singleton <$> Gen.unicode),
                (1, escapedUnicodeGen)
              ]
        )
  where
    escapedUnicodeGen :: Gen Text
    escapedUnicodeGen = do
      Text.append "\\u" <$> Gen.text (Range.singleton 4) Gen.hexit

jStringGen :: Gen JValue
jStringGen = JString <$> jsonStringGen (Range.constant 0 10)

jArrayGen :: Gen JValue
jArrayGen = JArray <$> Gen.list (Range.linear 0 10) jValueGen

jObjectGen :: Gen JValue
jObjectGen =
  JObject . HashMap.fromList <$> Gen.list (Range.linear 1 10) kvGen
  where
    kvGen = (,) <$> jsonStringGen (Range.constant 1 10) <*> jValueGen

jValueGen :: Gen JValue
jValueGen =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      jNullGen,
      jBoolGen,
      jNumberGen,
      jStringGen
    ]
    [ -- recursive generators
      jArrayGen,
      jObjectGen
    ]

jsonWhitespaceGen :: Gen Text
jsonWhitespaceGen =
  Gen.text (Range.constant 0 3) spaceGen
  where
    spaceGen = Gen.element [' ', '\n', '\r', '\t']

stringify :: JValue -> Gen Text
stringify = pad . go
  where
    surround l r j = l <> j <> r
    pad gen = surround <$> jsonWhitespaceGen <*> jsonWhitespaceGen <*> gen
    commaSeparated = pad . pure . Text.intercalate ","

    go value = case value of
      JArray elements ->
        traverse (pad . stringify) elements
          >>= fmap (surround "[" "]") . commaSeparated
      JObject kvs ->
        traverse stringifyKV (HashMap.toList kvs) >>= fmap (surround "{" "}") . commaSeparated
      _ -> return $ Text.pack (show value)

    stringifyKV (k, v) =
      surround <$> pad (pure $ Text.pack (showJSONString k)) <*> stringify v <*> pure ":"
