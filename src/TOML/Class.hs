{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module TOML.Class
  ( Value (..),
    IValue (..),
    Path (..),
    Key (..),
    Table (..),
    TomlAst (..),
    TOML,
  )
where

import Control.Exception (Exception)
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import qualified Data.Time as Time
import Data.Trie.Map (TMap)
import qualified Data.Trie.Map as TMap

newtype Path = Path {unPath :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, IsString)

newtype Key = Key {unKey :: NonEmpty Path}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable)

toPaths :: Key -> [Path]
toPaths = NonEmpty.toList . coerce

instance IsString Key where
  fromString "" = Key ("" :| [])
  fromString s =
    case Text.splitOn "." (Text.pack s) of
      [] -> error "Text.splitOn returned empty on a non-empty string"
      (x : xs) -> coerce @(NonEmpty Text) @Key (x :| xs)

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

-- | Allowed values inside inline tables.
data IValue
  = IValue Value
  | ITable Table
  deriving stock (Show, Eq)

newtype Table = Table {unTable :: [(Key, IValue)]}
  deriving stock (Show)
  deriving newtype (Eq)

data TomlAst
  = TableHeader Key
  | TableHeaderArray Key
  | KeyValue Key Value
  | InlineTable Key Table
  | InlineTableArray Key (NonEmpty Table)
  deriving stock (Show, Eq)

---------------------------------------------------------

type TOML = TMap Path Value

data TomlError
  = RepeatedHeader Key
  | DuplicatedKey Key
  deriving stock (Show)
  deriving anyclass (Exception)

data TomlState = TomlState
  { tables :: HashSet Key,
    currentTable :: Maybe Key
  }

emptyState :: TomlState
emptyState =
  TomlState
    { tables = HashSet.empty,
      currentTable = Nothing
    }

newtype TC a = TC {_runTC :: StateT TomlState (Except TomlError) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState TomlState,
      MonadError TomlError
    )

runTC :: TC a -> Either TomlError a
runTC = runExcept . flip evalStateT emptyState . _runTC

class (Monad m, MonadState TomlState m, MonadError TomlError m) => MonadTC m

instance MonadTC TC

-- Defining a key multiple times is not valid
typecheck :: [TomlAst] -> Either TomlError TOML
typecheck = runTC . go TMap.empty
  where
    go :: MonadTC m => TOML -> [TomlAst] -> m TOML
    go acc = \case
      (TableHeader key : asts) -> do
        insertTable key
        go acc asts
      _ -> undefined

    lookup :: Key -> TOML -> Maybe Value
    lookup key = TMap.lookup (toPaths key)

    -- Prevents repeated headers
    -- TODO: check header is not part of the TMap
    insertTable :: MonadTC m => Key -> m ()
    insertTable header = do 
      TomlState{..} <- get
      when (HashSet.member header tables) $ do
        throwError (RepeatedHeader header)
      put TomlState { 
        tables = HashSet.insert header tables,
        currentTable = Just header
      } 












