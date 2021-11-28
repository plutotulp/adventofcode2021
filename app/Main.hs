module Main where

import Options.Applicative

import qualified Meh

import Config

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
        <> progDesc "Run Advent of Code solution for the chosen day"
        <> header "aoc21" )

run :: Config -> IO ()
run _cfg = do
  putStrLn Meh.meh
