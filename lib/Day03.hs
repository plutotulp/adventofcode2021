module Day03
  ( main
  , test
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Data.Bits (Bits, (.&.), (.|.), bit, shiftR, testBit)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word16, Word32)
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.InterpolatedString.Perl6 (q,qc)
import Text.Megaparsec (some, parse)
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Task
import qualified Parsers

exampleInput :: Text
exampleInput =
  [q|00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010|]

exampleInputVals :: VU.Vector Word16
exampleInputVals = VU.fromList [4,30,22,23,21,15,7,28,16,25,2,10]

parser :: (VUM.Unbox w, Num w) => Parsers.Parser (VU.Vector w)
parser = VU.fromList <$> (space *> some (L.binary <* space))

getBit :: Bits a => a -> Int -> a
getBit x i = (x .&. bit i) `shiftR` i

addWordToOnesCount ::
  forall word vec m.
  ( Integral word, Bits word
  , PrimMonad m, VGM.MVector vec Int
  ) =>
  vec (PrimState m) Int -> word -> m ()
addWordToOnesCount ones x = do
  VGM.iforM_ ones $ \i count -> do
    let b = fromIntegral (getBit x i)
    VGM.write ones i (count + b)

countOnes ::
  forall word.
  ( Integral word, Bits word
  , VUM.Unbox word
  ) =>
  Int -> VU.Vector word -> VU.Vector Int
countOnes nbits vals = do
  runST $ do
    ones <- VGM.replicate nbits 0
    VG.mapM_ (addWordToOnesCount ones) vals
    VG.freeze ones

halfLength :: VG.Vector v a => v a -> Int
halfLength xs = VG.length xs `div` 2

bitsToWord ::
  ( Bits word
  , Num word
  , VG.Vector vec Bool
  ) => vec Bool -> word
bitsToWord = VG.ifoldl step 0
  where
    step res i b
      | b         = res .|. bit i
      | otherwise = res

-- gamma :: Int -> VU.Vector Int -> Word16
gamma ::
  forall word.
  ( Integral word
  , Bits word
  ) => Int -> VU.Vector Int -> word
gamma half ones = bitsToWord majority
  where
    majority = VG.map (> half) ones

epsilon ::
  forall word.
  ( Integral word
  , Bits word
  ) => Int -> VU.Vector Int -> word
epsilon half ones = bitsToWord minority
  where
    minority = VG.map (< half) ones

powerConsumption ::
  forall word1 word2.
  ( Integral word1, Bits word1, Num word2
  , VUM.Unbox word1
  ) =>
  Int -> VU.Vector word1 -> word2
powerConsumption nbits vals = fromIntegral g * fromIntegral e
  where
    g = gamma   @word1 half ones
    e = epsilon @word1 half ones
    ones = countOnes @word1 nbits vals
    half = halfLength vals

type BitCriteria = Int -> Int -> Bool

oxygenGeneratorBitCriteria :: BitCriteria
oxygenGeneratorBitCriteria zeroes ones = zeroes <= ones

co2ScrubberBitCriteria :: BitCriteria
co2ScrubberBitCriteria  zeroes ones = ones < zeroes

rating ::
  forall word.
  ( Bits word, Integral word
  , VUM.Unbox word
  ) =>
  Int -> VU.Vector word -> BitCriteria -> word
rating nbits vals crit = go msb vals
  where
    msb = nbits - 1
    go ib vs
      | VU.length vs == 1 = VU.head vs
      | otherwise =
        let ones   = countOnes (ib+1) vs VU.! ib
            zeroes = VU.length vs - ones
            c      = crit zeroes ones
            p v    = testBit v ib == c
        in go (ib - 1) (VU.filter p vs)

lifeSupportRating ::
  forall word1 word2.
  ( Num word2
  , Bits word1
  , Integral word1
  , VUM.Unbox word1
  ) =>
  Int -> VU.Vector word1 -> word2
lifeSupportRating nbits vals = fromIntegral co2 * fromIntegral oxy
  where
    oxy = rating nbits vals oxygenGeneratorBitCriteria
    co2 = rating nbits vals co2ScrubberBitCriteria

test :: IO ()
test = Task.hspec $ do

  describe "parser" $ do

    it "parses example input" $
      parse parser "" exampleInput `shouldParse` exampleInputVals

  describe "logic" $ do

    it "counts ones in example input" $
      countOnes 5 exampleInputVals
      `shouldBe` VU.fromList @Int [5,7,8,5,7]

    it "finds example gamma" $
      gamma @Word16
      (halfLength exampleInputVals)
      (countOnes 5 exampleInputVals)
      `shouldBe` 22

    it "calculates epsilon from example gamma" $
      epsilon @Word16
      (halfLength exampleInputVals)
      (countOnes 5 exampleInputVals)
      `shouldBe` 9

    it "calculates example power consumption" $
      powerConsumption @Word16 @Word32 5 exampleInputVals
      `shouldBe` 198

    it "calculates oxygen generator rating from example" $
      rating 5 exampleInputVals oxygenGeneratorBitCriteria
      `shouldBe` 23

    it "calculates oxygen generator rating from example" $
      rating 5 exampleInputVals co2ScrubberBitCriteria `shouldBe` 10

    it "calculates example life support rating" $
      lifeSupportRating @Word16 @Word32 5 exampleInputVals
      `shouldBe` 230

run1 :: VU.Vector Word16 -> IO ()
run1 vals =
  let p = powerConsumption @Word16 @Word32 12 vals
  in TextIO.putStrLn [qc|Power consumption: {p}|]

run2 :: VU.Vector Word16 -> IO ()
run2 vals =
  let r = lifeSupportRating @Word16 @Word32 12 vals
  in TextIO.putStrLn [qc|Life support rating: {r}|]

main :: Task.Main
main = Task.mkParsedMain test (parser, run1) (parser, run2)
