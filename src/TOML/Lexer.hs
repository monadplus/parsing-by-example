module TOML.Lexer
  ( parseZonedTime,
    parseLocalTime,
    parseTimeOfDay,
    parseDay,
  )
where

import Control.Applicative
import qualified Data.Time as T

parseZonedTime :: String -> Maybe T.ZonedTime
parseZonedTime =
  parseWith ["%FT%TZ", "%FT%T%z", "%FT%T%Q%z", "%FT%T%QZ", "%F %TZ", "%F %T%z", "%F %T%Q%z", "%F %T%QZ"]

parseLocalTime :: String -> Maybe T.LocalTime
parseLocalTime = parseWith ["%FT%T", "%FT%T%Q"]

parseTimeOfDay :: String -> Maybe T.TimeOfDay
parseTimeOfDay = parseWith ["%T", "%T%Q"]

parseDay :: String -> Maybe T.Day
parseDay = parseWith ["%F"]

parseWith :: T.ParseTime a => [String] -> String -> Maybe a
parseWith fmts str = foldr go Nothing fmts
  where
    go fmt acc = acc <|> T.parseTimeM False T.defaultTimeLocale fmt str
