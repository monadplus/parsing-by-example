{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative (optional)
import qualified Data.Text.IO as Text.IO
import JSON (ColumnWidth (..))
import qualified JSON as JSON
import Options.Applicative (Parser, ParserInfo)
import qualified Options.Applicative as Options
import qualified System.Exit as Exit
import qualified System.IO as IO

data Options = Options
  { input :: FilePath,
    -- | Defaults to stdin
    output :: Maybe FilePath
  }
  deriving stock (Show, Eq)

parser :: Parser Options
parser =
  Options
    <$> inputParser
    <*> optional outputParser
  where
    inputParser =
      Options.strOption
        ( Options.long "file"
            <> Options.short 'f'
            <> Options.metavar "FILENAME"
            <> Options.help "Input file to parse"
        )

    outputParser =
      Options.strOption
        ( Options.long "output"
            <> Options.short 'o'
            <> Options.metavar "FILENAME"
            <> Options.help "Output file to parse"
        )

parserInfo :: ParserInfo Options
parserInfo =
  Options.info
    (Options.helper <*> parser)
    (Options.progDesc "Command-line utility for parsing-by-example")

throws :: Either String a -> IO a
throws (Left err) = do
  IO.hPutStrLn IO.stderr err
  Exit.exitFailure
throws (Right a) = pure a

defaultColumnWidth :: ColumnWidth
defaultColumnWidth = ColumnWidth 80

main :: IO ()
main = do
  options <- Options.execParser parserInfo
  case options of
    Options {..} -> do
      text <- Text.IO.readFile input
      json <- throws (JSON.parse input text)
      case output of
        Nothing ->
          JSON.renderIO True IO.stdout defaultColumnWidth json
        Just file -> do
          IO.withFile file IO.WriteMode $ \handle ->
            JSON.renderIO False handle defaultColumnWidth json
