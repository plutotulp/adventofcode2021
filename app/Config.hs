module Config where

import Options.Applicative

import Data.Char (toUpper)
import Control.Monad (mzero, (<=<))
import GHC.Generics (Generic)
import Text.Read (readMaybe)

import Task

data Config =
  Config
  { day :: AdventDay
  , task :: Task
  , inputFile :: Maybe FilePath
  }
  deriving (Show, Generic)

newtype AdventDay = AdventDay Int
  deriving (Eq, Num, Show) via Int

readAdventDay :: String -> Maybe AdventDay
readAdventDay = check <=< readMaybe
  where
    check d
      | 1 <= d && d <= 24 = pure (AdventDay d)
      | otherwise = mzero

capitalize :: String -> String
capitalize = \case
  x:xs -> toUpper x : xs
  [] -> []

readTask :: String -> Maybe Task
readTask = readMaybe . capitalize

config :: Parser Config
config = let
  day' =
    option (maybeReader readAdventDay) $ mconcat
    [ short 'd'
    , long "day"
    , metavar "DAY"
    , help "Day number (1-24)"
    ]
  task' =
    option (maybeReader readTask) $ mconcat
    [ short 't'
    , long "task"
    , metavar "TASK"
    , help "Either test, run1 or run2"
    ]
  inputFile' =
    option (fmap Just str) $ mconcat
    [ short 'i'
    , long "input-file"
    , metavar "FILE"
    , help "Puzzle input"
    , value Nothing
    ]
  in Config <$> day' <*> task' <*> inputFile'
