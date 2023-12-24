{-# LANGUAGE OverloadedStrings #-}

module Test.TOML.Toml (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import TOML.Class
import Test.TOML.Common
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Toml"
    [ keyValueTests,
      tableTests,
      inlineTableTests,
      tableArrayTests,
      inlineTableArrayTests
    ]

keyValueTests :: TestTree
keyValueTests =
  testGroup
    "Key-value pairs"
    [ testCase "parse a key-value pair" $ do
        shouldParseToml "key=1" [KeyValue "key" int1]
        shouldParseToml "\"key\"=1" [KeyValue "key" int1]
        shouldParseToml "'key'=1" [KeyValue "key" int1]
        shouldParseToml "key1.key2=1" [KeyValue "key1.key2" int1]
        shouldParseToml "key= 1" [KeyValue "key" int1]
        shouldParseToml "key =1" [KeyValue "key" int1]
        shouldParseToml "key = 1" [KeyValue "key" int1]
        shouldParseToml "key=true" [KeyValue "key" (Bool True)]
        shouldParseToml "key=1.0" [KeyValue "key" float1]
        shouldParseToml "key=\"hello world\"" [KeyValue "key" (String "hello world")]
        shouldParseToml "key=1979-05-27T07:32:00Z" [KeyValue "key" zonedTime1]
        shouldParseToml "key=[1,2,3]" [KeyValue "key" (Array [int1, int2, int3])],
      testCase "ignore whitespaces around keys and values" $ do
        shouldParseToml "key=1   " [KeyValue "key" int1]
        shouldParseToml "   key=1" [KeyValue "key" int1]
        shouldParseToml "key=    1" [KeyValue "key" int1]
        shouldParseToml "key    =1" [KeyValue "key" int1]
        shouldParseToml "    key    =    1" [KeyValue "key" int1]
        shouldParseToml "key\t=1" [KeyValue "key" int1],
      testCase "fail when a key-value pair is not in the same line" $ do
        -- shouldFailParsingToml "key\n=1" -- FIXME:
        shouldFailParsingToml "key=\n1",
      testCase "allow values breaking lines" $ do
        shouldParseToml "key=\"\"\"hello\nworld\"\"\"" [KeyValue "key" (String "hello\nworld")]
        shouldParseToml "key=[1,\n2,\n3]" [KeyValue "key" (Array [int1, int2, int3])]
    ]

tableTests :: TestTree
tableTests =
  testGroup
    "Table"
    [ testCase "parse a table" $ do
        shouldParseToml
          "[table-1]\nkey1 = \"some string\"\nkey2 = 123"
          [TableHeader "table-1", KeyValue "key1" (String "some string"), KeyValue "key2" (Integer 123)]
        shouldParseToml
          "[table-1]\nkey1 = \"some string\"\nkey2 = 123\n[table-2]\nkey1 = \"some string\"\nkey2 = 123"
          [ TableHeader "table-1",
            KeyValue "key1" (String "some string"),
            KeyValue "key2" (Integer 123),
            TableHeader "table-2",
            KeyValue "key1" (String "some string"),
            KeyValue "key2" (Integer 123)
          ]
        shouldParseToml
          "[table]\nsub_table = { key1 = 'hello', key2.x = 3.14159}"
          [TableHeader "table", InlineTable "sub_table" (Table [("key1", istring "hello"), ("key2.x", ifloat 3.14159)])],
      testCase "parse a empty table" $ do
        shouldParseToml "[table]" [TableHeader "table"],
      testCase "allow comments between items and after items" $ do
        shouldParseToml
          "[fruits] # comment\n# another comment\nname = \"apple\" # another one"
          [ TableHeader "fruits",
            KeyValue "name" (String "apple")
          ]
    ]

inlineTableTests :: TestTree
inlineTableTests =
  testGroup
    "Inline table"
    [ testCase "parse an inline table" $ do
        shouldParseToml "name = { first = \"Tom\", last = \"Preston-Werner\"}" [InlineTable "name" (Table [("first", istring "Tom"), ("last", istring "Preston-Werner")])]
        shouldParseToml "point = { x = 1, y = 2 }" [InlineTable "point" (Table [("x", iinteger 1), ("y", iinteger 2)])],
      testCase "parse an empty inline table" $ do
        shouldParseToml "person.name = {}" [InlineTable "person.name" (Table [])],
      testCase "allow inline tables inside inline tables" $ do
        shouldParseToml "matrix = { a1 = { a11 = 1, a12 = 0 }, a2 = {a21=0,a22=1}}" [InlineTable "matrix" (Table [("a1", ITable (Table [("a11", iinteger 1), ("a12", iinteger 0)])), ("a2", ITable (Table [("a21", iinteger 0), ("a22", iinteger 1)]))])]
    ]

tableArrayTests :: TestTree
tableArrayTests =
  testGroup
    "Array of tables"
    [ testCase "parse an array of tables" $ do
        shouldParseToml
          "[[products]]\nname = \"Hammer\"\n\n[[products]]\nname=\"Nail\"\ncolor = \"gray\""
          [ TableHeaderArray "products",
            KeyValue "name" (String "Hammer"),
            TableHeaderArray "products",
            KeyValue "name" (String "Nail"),
            KeyValue "color" (String "gray")
          ],
      testCase "parse a subtable" $ do
        shouldParseToml
          "[[fruits]]\nname = \"apple\"\n[fruits.physical]\ncolor = \"red\""
          [ TableHeaderArray "fruits",
            KeyValue "name" (String "apple"),
            TableHeader "fruits.physical",
            KeyValue "color" (String "red")
          ],
      testCase "parse an empty aray of tables" $ do
        shouldParseToml "[[products]]" [TableHeaderArray "products"],
      testCase "allow comments between items and after items" $ do
        shouldParseToml
          "[[fruits]] # comment\n# another comment\nname = \"apple\" # another one"
          [ TableHeaderArray "fruits",
            KeyValue "name" (String "apple")
          ]
    ]

inlineTableArrayTests :: TestTree
inlineTableArrayTests =
  testGroup
    "Inline array of tables"
    [ testCase "parse an inlined array of tables" $ do
        shouldParseToml
          "points = [ { x = 1, y = 2}, { x = 1, y = 2}]"
          [ InlineTableArray "points" (Table [("x", iinteger 1), ("y", iinteger 2)] :| [Table [("x", iinteger 1), ("y", iinteger 2)]])
          ],
      testCase "allow newlines between tables" $ do
        shouldParseToml
          "points = [\n{x = 1, y = 2},\n{ x = 1, y = 2}\n]"
          [ InlineTableArray "points" (Table [("x", iinteger 1), ("y", iinteger 2)] :| [Table [("x", iinteger 1), ("y", iinteger 2)]])
          ]
    ]
