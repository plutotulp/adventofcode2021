module Day01
  ( main
  , test
  ) where

import Data.Text (Text)
import Data.Bool (bool)
import qualified Data.Text.IO as TextIO
import Test.Hspec (describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.InterpolatedString.Perl6 (q, qc)
import Text.Megaparsec (some, parse)
import Text.Megaparsec.Char (space)

import qualified Task
import qualified Parsers

exampleInput :: Text
exampleInput =
  [q|199
200
208
210
200
207
240
269
260
263|]

exampleInputVals :: [Int]
exampleInputVals =
  [ 199
  , 200
  , 208
  , 210
  , 200
  , 207
  , 240
  , 269
  , 260
  , 263
  ]

exampleInputValsWindowed :: [Int]
exampleInputValsWindowed =
  [ 607
  , 618
  , 618
  , 617
  , 647
  , 716
  , 769
  , 792
  ]

parser1 :: Parsers.Parser [Int]
parser1 = do
  res <- space *> some Parsers.decimal
  case res of
    xs@(_x1:_xs) -> pure xs
    [] -> fail "No input! Expected at least one depth measurement."

parser2 :: Parsers.Parser [Int]
parser2 = do
  res <- space *> some Parsers.decimal
  case res of
    xs@(_x1:_x2:_x3:_x4:_xs) -> pure xs
    _ -> fail "Too little input! Expected at least four depth measurements."

-- | Count the number of times the values in the input list increase
-- from the previous value. Assumes at least two entries in the list.
numIncreases :: [Int] -> Int
numIncreases ds = sum (bool 0 1 <$> zipWith (<) ds (tail ds))

run1 :: [Int] -> IO ()
run1 depths =
  TextIO.putStrLn [qc|Depth increased {numIncreases depths} times|]

-- | Sum list entries in sliding window of size 3. Assumes at least
-- three entries in the list.
windowed :: [Int] -> [Int]
windowed xs =
  zipWith3 add xs (tail xs) (tail (tail xs))
  where
    add a b c = a + b + c

run2 :: [Int] -> IO ()
run2 depths = do
  TextIO.putStrLn [qc|Windowed depth increased {numIncreases (windowed depths)} times|]

test :: IO ()
test = Task.hspec $ do
  describe "parser1" $ do
    it "parses example input" $
      parse parser1 "" exampleInput `shouldParse` exampleInputVals
  describe "logic" $ do
    it "windows input list" $
      windowed exampleInputVals == exampleInputValsWindowed
    it "finds seven increases in first example input" $
      numIncreases exampleInputVals == 7

main :: Task.Main
main = Task.mkParsedMain test (parser1, run1) (parser2, run2)
