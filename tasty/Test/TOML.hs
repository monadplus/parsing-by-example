{-# LANGUAGE OverloadedStrings #-}

module Test.TOML (tests) where

import qualified Test.TOML.Date as Date
import qualified Test.TOML.Integer as Integer
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "TOML"
    [ Date.tests,
      Integer.tests
    ]
