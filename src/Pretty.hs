{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty
  ( -- * Render
    renderStrict,
    renderIO,
    ColumnWidth (..),

    -- * Class
    Pretty (..),
  )
where

import Data.Scientific
import Data.Text (Text)
import GHC.IO.Handle (Handle)
import Prettyprinter (Doc, LayoutOptions, PageWidth (..), defaultLayoutOptions, layoutPageWidth)
import qualified Prettyprinter as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import qualified Prettyprinter.Render.Terminal as PP.Terminal
import qualified Prettyprinter.Render.Text as PP.Text

newtype ColumnWidth = ColumnWidth {column :: Int}

renderStrict :: Pretty a => Bool -> ColumnWidth -> a -> Text
renderStrict terminal columnWidth =
  render
    . PP.layoutSmart (mkLayoutOptions columnWidth)
    . pretty
  where
    render =
      if terminal
        then PP.Terminal.renderStrict
        else PP.Text.renderStrict

renderIO :: Pretty a => Bool -> Handle -> ColumnWidth -> a -> IO ()
renderIO terminal handle columnWidth =
  render handle
    . PP.layoutSmart (mkLayoutOptions columnWidth)
    . pretty
  where
    render =
      if terminal
        then PP.Terminal.renderIO
        else PP.Text.renderIO

mkLayoutOptions :: ColumnWidth -> LayoutOptions
mkLayoutOptions (ColumnWidth _column) =
  defaultLayoutOptions {layoutPageWidth = AvailablePerLine _column 1.0}

-- | 'Pretty.Pretty' for 'Doc AnsiStyle'
class Pretty a where
  pretty :: a -> Doc AnsiStyle

instance Pretty Double where
  pretty = PP.pretty

instance Pretty Scientific where
  pretty = PP.pretty . show

instance Pretty Int where
  pretty = PP.pretty

instance Pretty Integer where
  pretty = PP.pretty

instance Pretty Text where
  pretty = PP.pretty

instance Pretty () where
  pretty = PP.pretty

instance Pretty String where
  pretty = PP.pretty

instance Pretty (Doc AnsiStyle) where
  pretty = id
