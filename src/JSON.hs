module JSON
  ( -- * Type
    JValue (..),

    -- * Lexing
    lex,

    -- * Parsing
    parse,

    -- * Pretty
    renderStrict,
    renderIO,
    ColumnWidth (..),

    -- * Testing
    module JSON.Gen,
  )
where

import JSON.Class (JValue (..))
import JSON.Gen
import JSON.Lexer (lex)
import JSON.Parser (parse)
import Pretty (ColumnWidth (..), renderIO, renderStrict)
import Prelude hiding (lex)
