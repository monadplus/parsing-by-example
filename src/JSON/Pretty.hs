{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module JSON.Pretty
  ( -- * Render
    renderStrict,
    renderIO,
    ColumnWidth (..),

    -- * Class
    Pretty (..),

    -- * Style
    jsonLiteral,
    jsonKey,
    jsonNumber,
    jsonString,
  )
where

import Data.Scientific
import Data.Text (Text)
import GHC.IO.Handle (Handle)
import Prettyprinter (Doc, LayoutOptions, PageWidth (..), defaultLayoutOptions, layoutPageWidth)
import qualified Prettyprinter as Pretty
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..))
import qualified Prettyprinter.Render.Terminal as Pretty.Terminal
import qualified Prettyprinter.Render.Text as Pretty.Text

newtype ColumnWidth = ColumnWidth {column :: Int}

renderStrict :: Pretty a => Bool -> ColumnWidth -> a -> Text
renderStrict terminal columnWidth =
  render
    . Pretty.layoutSmart (mkLayoutOptions columnWidth)
    . pretty
  where
    render =
      if terminal
        then Pretty.Terminal.renderStrict
        else Pretty.Text.renderStrict

renderIO :: Pretty a => Bool -> Handle -> ColumnWidth -> a -> IO ()
renderIO terminal handle columnWidth =
  render handle
    . Pretty.layoutSmart (mkLayoutOptions columnWidth)
    . pretty
  where
    render =
      if terminal
        then Pretty.Terminal.renderIO
        else Pretty.Text.renderIO

mkLayoutOptions :: ColumnWidth -> LayoutOptions
mkLayoutOptions (ColumnWidth column) =
  defaultLayoutOptions {layoutPageWidth = AvailablePerLine column 1.0}

-- | 'Pretty.Pretty' for 'Doc AnsiStyle'
class Pretty a where
  pretty :: a -> Doc AnsiStyle

instance Pretty Double where
  pretty = Pretty.pretty

instance Pretty Scientific where
  pretty = Pretty.pretty . show

instance Pretty Int where
  pretty = Pretty.pretty

instance Pretty Integer where
  pretty = Pretty.pretty

instance Pretty Text where
  pretty = Pretty.pretty

instance Pretty () where
  pretty = Pretty.pretty

instance Pretty String where
  pretty = Pretty.pretty

instance Pretty (Doc AnsiStyle) where
  pretty = id

-- | JSON literals e.g. null, true, false
jsonLiteral :: Doc AnsiStyle -> Doc AnsiStyle
jsonLiteral = Pretty.annotate (Pretty.Terminal.colorDull Red)

jsonKey :: Doc AnsiStyle -> Doc AnsiStyle
jsonKey = Pretty.annotate (Pretty.Terminal.color Green)

jsonString :: Doc AnsiStyle -> Doc AnsiStyle
jsonString = Pretty.annotate (Pretty.Terminal.colorDull Yellow)

jsonNumber :: Doc AnsiStyle -> Doc AnsiStyle
jsonNumber = Pretty.annotate (Pretty.Terminal.colorDull Cyan)
