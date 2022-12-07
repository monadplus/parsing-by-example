{-# LANGUAGE OverloadedStrings #-}

module Test.TOML (tests) where

import qualified Test.TOML.Bool as Bool
import qualified Test.TOML.Date as Date
import qualified Test.TOML.Float as Float
import qualified Test.TOML.Integer as Integer
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "TOML"
    [ Bool.tests,
      Date.tests,
      Float.tests,
      Integer.tests
    ]
