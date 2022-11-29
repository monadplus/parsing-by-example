{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TOML.Class
  ( TValue (..),
  )
where

import qualified Data.Time as T
import Pretty (Pretty (..))

-- |
-- * Date and times follow RFC 3339.
data TValue
  = ZonedTime T.ZonedTime

instance Show TValue where
  show = undefined

instance Pretty TValue where
  pretty = undefined
