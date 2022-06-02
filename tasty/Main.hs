{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import qualified Data.Text as Text
import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog as Hedgehog
import JSON.Class (JValue)
import qualified JSON.Gen as Gen
import qualified JSON.Lexer as Lexer
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  defaultMain tests

tests, unitTests, propTests :: TestTree
tests =
  testGroup
    "Tests"
    [ unitTests,
      propTests
    ]
unitTests =
  testGroup
    "Unit Tests"
    []
propTests =
  testGroup
    "Property-based Tests"
    [ testPropertyNamed "lexer jstring" "prop_lexer_jstring" prop_lexer_jstring,
      testPropertyNamed "lexer jnumber" "prop_lexer_jnumber" prop_lexer_jnumber,
      testPropertyNamed "lexer jarray" "prop_lexer_jarray" prop_lexer_jarray,
      testPropertyNamed "lexer jarray" "prop_lexer_jarray_ws" prop_lexer_jarray_ws,
      testPropertyNamed "lexer jobject" "prop_lexer_jobject" prop_lexer_jobject,
      testPropertyNamed "lexer jobject" "prop_lexer_jobject_ws" prop_lexer_jobject_ws,
      testPropertyNamed "lexer jvalue" "prop_lexer_jvalue" prop_lexer_jvalue,
      testPropertyNamed "lexer jvalue" "prop_lexer_jvalue_ws" prop_lexer_jvalue_ws
    ]

prop_lexer_jstring :: Property
prop_lexer_jstring = prop_lexer_generic Gen.jStringGen

prop_lexer_jnumber :: Property
prop_lexer_jnumber = prop_lexer_generic Gen.jNumberGen

prop_lexer_jarray, prop_lexer_jarray_ws :: Property
prop_lexer_jarray = prop_lexer_generic Gen.jArrayGen
prop_lexer_jarray_ws = prop_lexer_stringify Gen.jArrayGen

prop_lexer_jobject, prop_lexer_jobject_ws :: Property
prop_lexer_jobject = prop_lexer_generic Gen.jObjectGen
prop_lexer_jobject_ws = prop_lexer_stringify Gen.jObjectGen

prop_lexer_jvalue, prop_lexer_jvalue_ws :: Property
prop_lexer_jvalue = prop_lexer_generic Gen.jValueGen
prop_lexer_jvalue_ws = prop_lexer_stringify Gen.jValueGen

prop_lexer_generic :: Gen JValue -> Property
prop_lexer_generic gen = property $ do
  jvalue <- forAll gen
  let r = Lexer.lex "" (Text.pack $ show jvalue)
  void $ Hedgehog.evalEither $ r

prop_lexer_stringify :: Gen JValue -> Property
prop_lexer_stringify gen = property $ do
  jvalue <- forAll gen
  stringified <- forAll (Gen.stringify jvalue)
  let r = Lexer.lex "" stringified
  void $ Hedgehog.evalEither $ r
