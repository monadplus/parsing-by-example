{-# LANGUAGE OverloadedStrings #-}

module Test.TOML (tests) where

import qualified Test.TOML.Date as Date
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "TOML"
    [ Date.tests
    ]
