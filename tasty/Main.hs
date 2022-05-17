module Main where

import qualified Data.List as List
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  defaultMain tests

tests, properties, unitTests, scProps :: TestTree
tests =
  testGroup
    "Tests"
    [ properties,
      unitTests
    ]
properties = testGroup "Properties" [scProps]
scProps =
  testGroup
    "(Checked by SmallCheck)"
    [ SC.testProperty "sort == sort . reverse" $
        \list -> List.sort (list :: [Int]) == List.sort (reverse list)
    ]
unitTests =
  testGroup
    "Unit tests"
    [ testCase "2+2=4" $
        2 + 2 @?= (4 :: Integer)
    ]
