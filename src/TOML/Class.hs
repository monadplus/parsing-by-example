{-# LANGUAGE DerivingStrategies #-}

module TOML.Class
  ( Value (..),
  )
where

import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import qualified Data.Time as Time

data Value
  = Bool Bool
  | Integer Integer
  | Float Double
  | String Text
  | ZonedTime ZonedTime
  | LocalTime LocalTime
  | Day Day
  | TimeOfDay TimeOfDay
  | Array [Value]
  deriving stock (Show)

instance Eq Value where
  (Bool b1) == (Bool b2) = b1 == b2
  (Integer i1) == (Integer i2) = i1 == i2
  (Float f1) == (Float f2)
    | isNaN f1 && isNaN f2 = True
    | otherwise = f1 == f2
  (String s1) == (String s2) = s1 == s2
  (ZonedTime a) == (ZonedTime b) =
    Time.zonedTimeToUTC a == Time.zonedTimeToUTC b
  (LocalTime a) == (LocalTime b) = a == b
  (Day a) == (Day b) = a == b
  (TimeOfDay a) == (TimeOfDay b) = a == b
  (Array a1) == (Array a2) = a1 == a2
  _ == _ = False
