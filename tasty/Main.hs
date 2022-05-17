module Main where

import System.IO (hSetEncoding, stderr, stdout, utf8)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "2+2=4" $
        2 + 2 @?= 4
    ]
