module Main where

import Options.Applicative (execParser, info, (<**>), fullDesc, progDesc, header, helper)

import Config

import qualified Task
import qualified Day01
import qualified Day02
import qualified Day03

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
    err = error "Missing input file, fix config parser to react sooner, plz"
    runSetup doTask =
      case day cfg of
        1 -> Day01.main doTask
        2 -> Day02.main doTask
        3 -> Day03.main doTask
        _ -> error "missing impl"
