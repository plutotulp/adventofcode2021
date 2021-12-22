module Day11
  ( main
  , test
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Char (digitToInt)
import Data.Foldable (for_)
import Data.Generics.Labels ()
import Data.List (sort)
import Data.STRef (newSTRef, readSTRef, modifySTRef)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified  Data.Vector.Generic as VG
import qualified  Data.Vector.Generic.Mutable as VGM
import qualified  Data.Vector.Unboxed as VU
import Data.Word (Word8, Word64)
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.InterpolatedString.Perl6 (q, qc)
import Text.Megaparsec (parse, some, many)
import Text.Megaparsec.Char (digitChar, space)

import qualified Task
import qualified Parsers

exampleInput :: Text
exampleInput = [q|5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526|]

type Row = Int
type Col = Int
type Dims = (Col, Row)
type Point = (Col, Row)
type Energy = VU.Vector Word8
type Input = (Dims, Energy)

exampleInputVals :: Input
exampleInputVals = ((10, 10), [5,4,8,3,1,4,3,2,2,3,2,7,4,5,8,5,4,7,1,1,5,2,6,4,5,5,6,1,7,3,6,1,4,1,3,3,6,1,4,6,6,3,5,7,3,8,5,4,7,8,4,1,6,7,5,2,4,6,4,5,2,1,7,6,8,4,1,7,2,1,6,8,8,2,8,8,1,1,3,4,4,8,4,6,8,4,8,5,5,4,5,2,8,3,7,5,1,5,2,6])

-- Note, does not consume trailing space, just a single digit.
parseWord8' :: Parsers.Parser Word8
parseWord8' = fromIntegral . digitToInt <$> digitChar

parseRow :: Int -> Parsers.Parser Energy
parseRow nc = VG.replicateM nc parseWord8' <* space

parseFirstRow :: Parsers.Parser (Col, Energy)
parseFirstRow = do
  cs <- VG.fromList <$> some parseWord8'
  space
  pure (VG.length cs, cs)

parser :: Parsers.Parser (Dims, Energy)
parser = do
  space
  (nc, r0) <- parseFirstRow <* space
  rs <- many (parseRow nc <* space)
  let vec = VG.concat (r0 : rs)
      nr  = VG.length vec `div` nc
  pure ((nc, nr), vec)

neighbors :: Dims -> Point -> [Point]
neighbors (nc, nr) (c0, r0) =
  [ x
  | x@(c, r) <-
      [ (c0-1, r0-1), (c0  , r0-1), (c0+1, r0-1)
      , (c0-1, r0  ),               (c0+1, r0  )
      , (c0-1, r0+1), (c0  , r0+1), (c0+1, r0+1)
      ]
  , 0 <= r
  , 0 <= c
  , r < nr
  , c < nc
  ]

-- point with neighbors
neighborhood :: Dims -> Point -> [Point]
neighborhood dims p = p : neighbors dims p

size :: Dims -> Int
size (nc, nr) = fromIntegral nc * fromIntegral nr

index :: Col -> Point -> Int
index nc (c, r) = r * nc + c

locToPoint :: Col -> Int -> Point
locToPoint nc loc = (c, r)
  where
   (r, c) = divMod loc nc

modelStep :: Dims -> VU.MVector s Word8 -> ST s Word64
modelStep dims@(nc, _) es = do
  count <- newSTRef 0
  VGM.iforM_ es (update count)
  VGM.iforM_ es maybeZero
  readSTRef count
  where
    update count loc (succ -> val) = do
      VGM.write es loc val
      maybeFire count loc val

    maybeFire count loc val = do
      when (val == 10) $ do
        modifySTRef count succ
        fire count loc

    fire count loc = do
      for_ (neighbors dims (locToPoint nc loc)) $ \pt -> do
        let nloc = index nc pt
        val <- succ <$> VGM.read es nloc
        VGM.write es nloc val
        maybeFire count nloc val

    maybeZero loc val = when (9 < val) (VGM.write es loc 0)

runModelSteps :: Int -> Dims -> VU.MVector s Word8 -> ST s Word64
runModelSteps n dims es = fmap sum (sequence (replicate n (modelStep dims es)))

runModelStepsAndCountFlashes :: Int -> Input -> Word64
runModelStepsAndCountFlashes n (dims, vals) = runST $ do
  vec <- VG.thaw vals
  runModelSteps n dims vec

findFirstSynchronizedFlashStep :: Input -> Int
findFirstSynchronizedFlashStep (dims, vals) = runST $ do
  vec <- VG.thaw vals
  let sz = size dims
      go (succ -> i) = do
        n <- modelStep dims vec
        if fromIntegral n == sz then pure i else go i
  go 0

test :: IO ()
test = Task.hspec $ do

  describe "parser" $ do

    it "parses example input" $
      parse parser "" exampleInput `shouldParse` exampleInputVals

  describe "logic" $ do

    it "finds full neighborhood" $ do
      sort (neighborhood (5, 5) (2, 2)) `shouldBe`
        sort
        [ (1,1),(2,1),(3,1)
        , (1,2),(2,2),(3,2)
        , (1,3),(2,3),(3,3)
        ]

    it "finds upper left corner neighbouhood" $ do
      sort (neighborhood (12, 3) (0, 0)) `shouldBe`
        sort
        [ (0,0),(1,0)
        , (0,1),(1,1)
        ]

    it "finds bottom right corner neighborhood" $ do
      sort (neighborhood (2, 7) (1, 6)) `shouldBe`
        sort
        [ (0,5),(1,5)
        , (0,6),(1,6)
        ]

    it "finds left edge neighborhood" $ do
      sort (neighborhood (100, 1200) (0, 78)) `shouldBe`
        sort
        [ (0,77), (1,77)
        , (0,78), (1,78)
        , (0,79), (1,79)
        ]

run1 :: Input -> IO ()
run1 input =
  TextIO.putStrLn [qc|Fired {nfired} times while running {nstep} steps.|]
  where
    nstep  = 100
    nfired = runModelStepsAndCountFlashes nstep input

run2 :: Input -> IO ()
run2 input =
  TextIO.putStrLn [qc|We get the first synchronized flash in step {step}.|]
  where
    step = findFirstSynchronizedFlashStep input

main :: Task.Main
main = Task.mkParsedMain test (parser, run1) (parser, run2)
