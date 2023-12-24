{-# LANGUAGE OverloadedStrings #-}

module Test.TOML.String (tests) where

import Test.TOML.Common (dQuote, quote, shouldFailParsingString, shouldParseString, tripleDQuote, tripleQuote)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "String"
    [ basicStringTests,
      literalStringTests,
      multiLineBasicStringTests,
      multiLineLiteralStringTests
    ]

basicStringTests :: TestTree
basicStringTests =
  testGroup
    "Basic string"
    [ testCase "parse strings surrounded by double quotes" $ do
        shouldParseString (dQuote "") ""
        shouldParseString (dQuote "a") "a"
        shouldParseString (dQuote "word") "word"
        shouldParseString (dQuote "word1 word2") "word1 word2"
        shouldFailParsingString (dQuote "\"a")
        shouldFailParsingString (dQuote "a\""),
      testCase "parse strings with escape characters e.g. newline, backslash, and unicodes" $ do
        shouldParseString (dQuote "backspace: \\b") "backspace: \b"
        shouldParseString (dQuote "tab: \\t") "tab: \t"
        shouldParseString (dQuote "linefeed: \\n") "linefeed: \n"
        shouldParseString (dQuote "form feed: \\f") "form feed: \f"
        shouldParseString (dQuote "carriage return: \\r") "carriage return: \r"
        shouldParseString (dQuote "quote: \\\"") "quote: \""
        shouldParseString (dQuote "backslash: \\\\") "backslash: \\"
        shouldParseString (dQuote "Unicode 4: \\u24B6") "Unicode 4: \9398"
        shouldParseString (dQuote "Unicode 8: \\U0001f618") "Unicode 8: \128536"
        shouldParseString (dQuote "I'm a string. \\\"You can quote me\\\". Name\\tJos\\u00E9\\nLocation\\tSF.") "I'm a string. \"You can quote me\". Name\tJos\233\nLocation\tSF.",
      testCase "fail on non-escaped control characters" $ do
        shouldFailParsingString "Hello \n world"
        shouldFailParsingString "Hello \t world",
      testCase "fail on invalid escpaed control characters" $ do
        shouldFailParsingString "Hello \\z world!",
      testCase "fail on multi-line strings" $ do
        shouldFailParsingString "Hello \n world!"
        shouldFailParsingString "Hello \r\n world!"
        shouldFailParsingString "\nHello world!"
        shouldFailParsingString "Hello world!\n",
      testCase "fail on invalid unicodes" $ do
        shouldFailParsingString "\\u0"
        shouldFailParsingString "\\U24B6"
        shouldFailParsingString "\\U1F618"
    ]

literalStringTests :: TestTree
literalStringTests =
  testGroup
    "Literal string"
    [ testCase "parse string surrounded by quotes" $ do
        shouldParseString (quote "C:\\Users\\nodejs\\templates") "C:\\Users\\nodejs\\templates"
        shouldParseString (quote "Tom \"Dubs\" Preston-Werner") "Tom \"Dubs\" Preston-Werner",
      testCase "fail on newlines" $ do
        shouldFailParsingString (quote "Roses are \n red")
        shouldFailParsingString (quote "Violes are \n\r blue")
    ]

multiLineBasicStringTests :: TestTree
multiLineBasicStringTests =
  testGroup
    "Multi-line basic string"
    [ testCase "parse string surrounded by triple double-quotes" $ do
        shouldParseString (tripleDQuote "Roses are red") "Roses are red",
      testCase "parse strings with escape characters e.g. newline, backslash, and unicodes" $ do
        shouldParseString (tripleDQuote "backspace: \\b") "backspace: \b"
        shouldParseString (tripleDQuote "tab: \\t") "tab: \t"
        shouldParseString (tripleDQuote "linefeed: \\n") "linefeed: \n"
        shouldParseString (tripleDQuote "form feed: \\f") "form feed: \f"
        shouldParseString (tripleDQuote "carriage return: \\r") "carriage return: \r"
        shouldParseString (tripleDQuote "quote: \\\"") "quote: \""
        shouldParseString (tripleDQuote "backslash: \\\\") "backslash: \\"
        shouldParseString (tripleDQuote "Unicode 4: \\u24B6") "Unicode 4: \9398"
        shouldParseString (tripleDQuote "Unicode 8: \\U0001f618") "Unicode 8: \128536"
        shouldParseString (tripleDQuote "I'm a string. \\\"You can quote me\\\". Name\\tJos\\u00E9\\nLocation\\tSF.") "I'm a string. \"You can quote me\". Name\tJos\233\nLocation\tSF.",
      testCase "preserve newlines and whitespaces" $ do
        shouldParseString (tripleDQuote "Roses are red\n  Violtes are blue") "Roses are red\n  Violtes are blue",
      testCase "trim newline after opening delimiter" $ do
        shouldParseString (tripleDQuote "\nRoses are red\nVioltes are blue") "Roses are red\nVioltes are blue",
      testCase "trim whitespaces after \"line ending backslash\"" $ do
        shouldParseString (tripleDQuote "The quick brown \\\n\n  fox jumps over \\\n    the lazy dog.") "The quick brown fox jumps over the lazy dog."
        shouldParseString (tripleDQuote "\\\n    The quick brown \\\n    fox jumps over \\\n    the lazy dog.\\\n") "The quick brown fox jumps over the lazy dog.",
      testCase "allow at most two consecutives quotation marks" $ do
        shouldParseString (tripleDQuote "Here are two quotation marks: \"\". Simple enough.") "Here are two quotation marks: \"\". Simple enough."
        shouldParseString (tripleDQuote "Here are three quotation marks: \"\"\\\".") "Here are three quotation marks: \"\"\"."
        shouldFailParsingString (tripleDQuote "Here are three quotation marks: \"\"\".")
        shouldParseString (tripleDQuote "Hello \"\" World!") "Hello \"\" World!"
    ]

multiLineLiteralStringTests :: TestTree
multiLineLiteralStringTests =
  testGroup
    "Multi-line literal string"
    [ testCase "parse string surrounded by triple quotes" $ do
        shouldParseString (tripleQuote "Roses are red") "Roses are red",
      testCase "preserve newlines and whitespaces" $ do
        shouldParseString (tripleQuote "Roses are red\n  Violtes are blue") "Roses are red\n  Violtes are blue",
      testCase "trim newline after opening delimiter" $ do
        shouldParseString (tripleQuote "\nRoses are red\nVioltes are blue") "Roses are red\nVioltes are blue",
      testCase "allow many quotation marks" $ do
        shouldParseString (tripleQuote "Here are 5 quotation marks: \"\"\"\"\"") "Here are 5 quotation marks: \"\"\"\"\"",
      testCase "fail on any control character but tab" $ do
        shouldParseString (tripleQuote "Tab: \t") "Tab: \t"
        shouldFailParsingString (tripleQuote "Backspace: \b")
        -- TODO: see multiLineStringP
        -- testCase "allow at most two consecutives single quotes marks" $ do
        --   shouldParseString (tripleQuote "'That, ' she said, 'is still pointless.'") "'That, ' she said, 'is still pointless.'",
    ]
