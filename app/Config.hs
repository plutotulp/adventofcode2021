{-# LANGUAGE RecordWildCards, ApplicativeDo #-}

module Config where

import GHC.Generics (Generic)
import Options.Applicative

data Config =
  Config
  { day :: Int
  }
  deriving (Show, Generic)

config :: Parser Config
config = do
  day <-
    option auto (short 'd' <> long "day" <> metavar "DAY" <> help "Day number (1-24)")
  pure (Config {..})
