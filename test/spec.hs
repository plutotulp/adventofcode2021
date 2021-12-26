module Main where

import Test.Hspec (hspec, context, parallel)

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
import qualified Day11
import qualified Day12
import qualified Day13

main :: IO ()
main = hspec $ parallel $ do
  context "day 01" Day01.spec
  context "day 02" Day02.spec
  context "day 03" Day03.spec
  context "day 04" Day04.spec
  context "day 05" Day05.spec
  context "day 06" Day06.spec
  context "day 07" Day07.spec
  context "day 08" Day08.spec
  context "day 09" Day09.spec
  context "day 10" Day10.spec
  context "day 11" Day11.spec
  context "day 12" Day12.spec
  context "day 13" Day13.spec
