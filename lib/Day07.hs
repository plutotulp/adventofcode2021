module Day07
  ( main
  , test
  , spec
  ) where

import Control.Arrow ((&&&))
import Data.Foldable (minimumBy)
import Data.Monoid (Sum(Sum, getSum))
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified  Data.Vector.Generic as VG
import qualified  Data.Vector.Unboxed as VU
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.InterpolatedString.Perl6 (qc)
import Text.Megaparsec (parse, sepBy1)
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Task
import qualified Parsers

exampleInput :: Text
exampleInput =
  "16,1,2,0,4,2,7,1,2,14"

exampleInputVals :: VU.Vector Word
exampleInputVals =
  VG.fromList [16,1,2,0,4,2,7,1,2,14]

parser :: Parsers.Parser (VU.Vector Word)
parser = VG.fromList <$> (space *> sepBy1 decimal comma)
  where
    decimal = L.lexeme space L.decimal
    comma   = L.symbol space ","

linCost :: Int -> Int -> Int
linCost p h = abs (p - h)

triCost :: Int -> Int -> Int
triCost p h = n * (n + 1) `div` 2
  where
    n = linCost p h

fuelCostAt :: (Int -> Int -> Int) -> Word -> VU.Vector Word -> Int
fuelCostAt cost (fromIntegral -> h) = getSum . VG.foldMap f
  where
    f (fromIntegral -> p) = Sum (cost p h)

minFuelCost :: (Int -> Int -> Int) -> VU.Vector Word -> (Word, Int)
minFuelCost f ps = minimumBy (comparing snd) posAndCosts
  where
    posAndCosts :: [(Word, Int)]
    posAndCosts = (id &&& flip (fuelCostAt f) ps) <$> [pmin .. pmax]
    pmin = VG.minimum ps
    pmax = VG.maximum ps

test :: IO ()
test = Task.hspec spec

spec :: Spec
spec = do
  describe "parser" $ do

    it "parses example input" $ do
      parse parser "" exampleInput `shouldParse` exampleInputVals

  describe "logic" $ do

    it "calculates example fuel cost at position 2" $ do
      fuelCostAt linCost 2 exampleInputVals `shouldBe` 37

    it "calculates example fuel cost at position 1" $ do
      fuelCostAt linCost 1 exampleInputVals `shouldBe` 41

    it "calculates example fuel cost at position 3" $ do
      fuelCostAt linCost 3 exampleInputVals `shouldBe` 39

    it "calculates example fuel cost at position 10" $ do
      fuelCostAt linCost 10 exampleInputVals `shouldBe` 71

    it "finds example pos with minimum fuel cost" $ do
      minFuelCost linCost exampleInputVals `shouldBe` (2, 37)

run1 :: VU.Vector Word -> IO ()
run1 ps = do
  let (pos, cost) = minFuelCost linCost ps
  TextIO.putStrLn [qc|Minimum fuel cost is {cost}, at position {pos}.|]

run2 :: VU.Vector Word -> IO ()
run2 ps = do
  let (pos, cost) = minFuelCost triCost ps
  TextIO.putStrLn [qc|Minimum fuel cost is {cost}, at position {pos}.|]

main :: Task.Main
main = Task.mkParsedMain test (parser, run1) (parser, run2)
