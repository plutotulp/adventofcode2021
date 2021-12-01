module Main where

import Options.Applicative (execParser, info, (<**>), fullDesc, progDesc, header, helper)

import Config

import qualified Task
import qualified Day01

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
        <> progDesc "Run Advent of Code solution for the chosen day"
        <> header "aoc21" )

run :: Config -> IO ()
run cfg =
  maybe err runSetup mkSetup
  where
    mkSetup = Task.toDoTask (task cfg) (inputFile cfg)
    err = error "Bad input, fix config parser to react sooner, plz"
    runSetup doTask =
      case day cfg of
        1 -> Day01.main doTask
        _ -> error "missing impl"
