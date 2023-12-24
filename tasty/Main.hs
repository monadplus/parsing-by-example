{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import System.IO (hSetEncoding, stderr, stdout, utf8)
import qualified Test.JSON
import qualified Test.TOML
import Test.Tasty

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Test.JSON.tests,
      Test.TOML.tests
    ]
