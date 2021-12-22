module Day06
  ( main
  , test
  ) where

import Control.Monad.ST (runST)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified  Data.Vector.Generic as VG
import qualified  Data.Vector.Generic.Mutable as VGM
import qualified  Data.Vector.Unboxed as VU
import Data.Word (Word64)
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.InterpolatedString.Perl6 (qc)
import Text.Megaparsec (parse, sepBy1)
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Task
import qualified Parsers

exampleInput :: Text
exampleInput =
  "3,4,3,1,2"

exampleInputVals :: [Word]
exampleInputVals =
  [3,4,3,1,2]

encode :: [Word] -> VU.Vector Word64
encode school = runST $ do
  acc <- VGM.replicate 9 0
  traverse_ (VGM.modify acc succ . fromIntegral) school
  VG.freeze acc

next :: VU.Vector Word64 -> VU.Vector Word64
next state = runST $ do
  acc <- VGM.replicate 9 0
  VG.iforM_ (VG.tail state) $ \i v -> VGM.write acc i v
  let nspawn = VG.head state
  VGM.modify acc (+nspawn) 6
  VGM.modify acc (+nspawn) 8
  VG.freeze acc

countAfter :: Word64 -> [Word] -> Word64
countAfter days = VG.sum . (!! fromIntegral days) . iterate next . encode

parser :: Parsers.Parser [Word]
parser = space *> sepBy1 decimal comma
  where
    decimal = L.lexeme space L.decimal
    comma   = L.symbol space ","

test :: IO ()
test = Task.hspec $ do

  describe "parser" $ do

    it "parses example input" $ do
      parse parser "" exampleInput `shouldParse` exampleInputVals

  describe "logic" $ do

    it "encodes example" $ do
      encode exampleInputVals `shouldBe`
        VU.fromList [0, 1, 1, 2, 1, 0, 0, 0, 0]

    it "spawns at 6 and 8" $ do
      next (VU.fromList [2, 2, 5, 0, 2, 9, 12, 20, 30]) `shouldBe`
        VU.fromList [2, 5, 0, 2, 9, 12, 22, 30, 2]

    it "finds the number of fish in the first example" $ do
      countAfter 80 exampleInputVals `shouldBe` 5934

    it "finds the number of fish in the second example" $ do
      countAfter 256 exampleInputVals `shouldBe` 26984457539

run1 :: [Word] -> IO ()
run1 fish = do
  let nd = 80
      nf = countAfter nd fish
  TextIO.putStrLn [qc|After {nd} days, there will be {nf} fish.|]

run2 :: [Word] -> IO ()
run2 fish = do
  let nd = 256
      nf = countAfter nd fish
  TextIO.putStrLn [qc|After {nd} days, there will be {nf} fish.|]

main :: Task.Main
main = Task.mkParsedMain test (parser, run1) (parser, run2)
