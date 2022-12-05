{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TOML.Class
  ( Value (..),
  )
where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import qualified Data.Time as Time

data Value
  = Bool Bool
  | Integer Int
  | Double Scientific
  | Text Text
  | ZonedTime ZonedTime
  | LocalTime LocalTime
  | Day Day
  | TimeOfDay TimeOfDay
  | Array [Value]
  deriving stock (Show)

instance Eq Value where
  (Bool b1) == (Bool b2) = b1 == b2
  (Integer i1) == (Integer i2) = i1 == i2
  (Double f1) == (Double f2) = f1 == f2
  (Text s1) == (Text s2) = s1 == s2
  (ZonedTime a) == (ZonedTime b) =
    Time.zonedTimeToUTC a == Time.zonedTimeToUTC b
  (LocalTime a) == (LocalTime b) = a == b
  (Day a) == (Day b) = a == b
  (TimeOfDay a) == (TimeOfDay b) = a == b
  (Array a1) == (Array a2) = a1 == a2
  _ == _ = False
