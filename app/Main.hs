module Main where

import Options.Applicative (execParser, info, (<**>), fullDesc, progDesc, header, helper)

import Config

import qualified Task
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10

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
      let f =  case day cfg of
            1 -> Day01.main
            2 -> Day02.main
            3 -> Day03.main
            4 -> Day04.main
            5 -> Day05.main
            6 -> Day06.main
            7 -> Day07.main
            8 -> Day08.main
            9 -> Day09.main
            10 -> Day10.main
            _ -> error "missing impl"
      in f doTask
