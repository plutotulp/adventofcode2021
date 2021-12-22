module Day09
  ( main
  , test
  ) where

import Control.Monad (unless)
import Control.Monad.ST (ST, runST)
import Data.Char (digitToInt)
import Data.Foldable (for_)
import Data.Generics.Labels ()
import Data.List (sort)
import Data.Monoid (Sum(Sum, getSum))
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
exampleInput = [q|2199943210
3987894921
9856789892
8767896789
9899965678|]

type Row = Int
type Col = Int
type Dims = (Col, Row)
type Point = (Col, Row)
type Labels  = VU.Vector Word8
type Heights = VU.Vector Word8
type Input = (Dims, Heights)


exampleInputVals :: Input
exampleInputVals = ((10, 5), [2,1,9,9,9,4,3,2,1,0,3,9,8,7,8,9,4,9,2,1,9,8,5,6,7,8,9,8,9,2,8,7,6,7,8,9,6,7,8,9,9,8,9,9,9,6,5,6,7,8])

-- Note, does not consume trailing space, just a single digit.
parseWord8' :: Parsers.Parser Word8
parseWord8' = fromIntegral . digitToInt <$> digitChar

parseRow :: Int -> Parsers.Parser Heights
parseRow nc = VG.replicateM nc parseWord8' <* space

parseFirstRow :: Parsers.Parser (Col, Heights)
parseFirstRow = do
  cs <- VG.fromList <$> some parseWord8'
  space
  pure (VG.length cs, cs)

parser :: Parsers.Parser (Dims, Heights)
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
      [               (c0  , r0-1)
      , (c0-1, r0  ),               (c0+1, r0  )
      ,               (c0  , r0+1)
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

range :: Dims -> [Point]
range (nc, nr) = [(c, r) | r <- [0..(nr-1)], c <- [0..(nc-1)] ]

isLowPoint :: Input -> Point -> Bool
isLowPoint (sz@(nc, _),  mp) pos = all (v <) vs
  where
    is = index nc <$> neighbors sz pos
    vs = (mp VG.!) <$> is
    v  = mp VG.! index nc pos

lowPoints :: Input -> [Point]
lowPoints input@(sz, _) = filter (isLowPoint input) (range sz)

lowPointRiskLevels :: Input -> [Word8]
lowPointRiskLevels input@((nc, _), mp) = f <$> lowPoints input
  where
    f p = 1 + mp VG.! index nc p

totalLowPointRiskLevel  :: Input -> Word64
totalLowPointRiskLevel =
  getSum . foldMap (Sum . fromIntegral) . lowPointRiskLevels

noLabel :: Word8
noLabel = 0

labelPoints :: Dims -> [Point] -> Labels
labelPoints dims@(nc, _) ps = runST $ do
  lab <- newSTRef noLabel
  vec <- VGM.replicate (size dims) 0
  for_ ps $ \p -> do
    modifySTRef lab succ
    v <- readSTRef lab
    VGM.write vec (index nc p) v
  VG.unsafeFreeze vec

mkLabels :: Dims -> [Point] -> ST s (VU.MVector s Word8)
mkLabels dims@(nc, _) ps = do
  labNo <- newSTRef noLabel
  vec <- VGM.replicate (size dims) 0
  for_ ps $ \p -> do
    modifySTRef labNo succ
    lab <- readSTRef labNo
    VGM.write vec (index nc p) lab
  pure vec

countLabelled :: Labels -> Word64
countLabelled = getSum . VG.foldMap count
  where
    count v | v == 0    = Sum 0
            | otherwise = Sum 1

topHeight :: Word8
topHeight = 9

-- If given point has a label, grow the label into neighbors (if
-- possible).
growPoint :: Dims -> Heights -> VU.MVector s Word8 -> Point -> ST s ()
growPoint dims@(nc, _) hs ls p = do
  v <- VGM.read ls (index nc p)
  unless (v == noLabel) $ do
    let nps = neighbors dims p
    mapM_ (growLabelPoint dims hs ls v) nps

-- Label point with the given label, unless
--
--   1. Already labelled, in which case the label with highest
--      value wins.
--   2. Height is topHeight, in which case point cannot be labelled.
growLabelPoint ::
  Dims -> Heights -> VU.MVector s Word8 -> Word8 -> Point -> ST s ()
growLabelPoint (nc, _) hs ls label point = do
  let loc = index nc point
  let height = hs VG.! loc
  unless (height == topHeight) $ do
    oldLabel <- VGM.read ls loc
    VGM.write ls loc (max label oldLabel)

labelledPoints :: Dims -> Labels -> [Point]
labelledPoints (nc, _) = VG.ifoldl step []
  where
    step ps loc v
      | v == noLabel = ps
      | otherwise    = locToPoint nc loc : ps

growBasins :: Dims -> Heights -> VU.MVector s Word8 -> ST s ()
growBasins dims hs ls = go 0
  where
    go n0 = do
      ps <- labelledPoints dims <$> VG.unsafeFreeze ls
      mapM_ (growPoint dims hs ls) ps
      n1 <- countLabelled <$> VG.unsafeFreeze ls
      if n0 == n1 then pure () else go n1

basins :: Input -> Labels
basins input@(dims, hs) = runST $ do
  ls <- mkLabels dims (lowPoints input)
  growBasins dims hs ls
  VG.unsafeFreeze ls

basinSizes :: Labels -> VU.Vector Word64
basinSizes ls = runST $ do
  sizes <- VGM.replicate (fromIntegral (VG.maximum ls)) 0
  VG.forM_ ls $ \v -> do
    unless (v == noLabel) $ do
      VGM.modify sizes succ (pred (fromIntegral v))
  VG.unsafeFreeze sizes

-- extract maximum value and replace it with minBound
popMaxval :: (VG.Vector v a, Bounded a, Ord a, Num a) => VG.Mutable v s a -> ST s a
popMaxval vec = do
  loc <- VG.maxIndex <$> VG.unsafeFreeze vec
  val <- VGM.read vec loc
  VGM.write vec loc minBound
  pure val

basinsChecksum :: Input -> Word64
basinsChecksum input = s1 * s2 * s3
  where
    (s1, s2, s3) = runST $ do
      sz <- VG.unsafeThaw (basinSizes (basins input))
      (,,) <$> popMaxval sz <*> popMaxval sz <*> popMaxval sz

test :: IO ()
test = Task.hspec $ do

  describe "parser" $ do

    it "parses example input" $
      parse parser "" exampleInput `shouldParse` exampleInputVals

    it "parses first row of example input" $
      parse parseFirstRow "" exampleInput `shouldParse` (10, [2,1,9,9,9,4,3,2,1,0])

  describe "logic" $ do

    it "finds full neighborhood" $ do
      sort (neighborhood (5, 5) (2, 2)) `shouldBe`
        sort
        [       (2,1)
        , (1,2),(2,2),(3,2)
        ,       (2,3)
        ]

    it "finds upper left corner neighbouhood" $ do
      sort (neighborhood (12, 3) (0, 0)) `shouldBe`
        sort
        [ (0,0),(1,0)
        , (0,1)
        ]

    it "finds bottom right corner neighborhood" $ do
      sort (neighborhood (2, 7) (1, 6)) `shouldBe`
        sort
        [       (1,5)
        , (0,6),(1,6)
        ]

    it "finds left edge neighborhood" $ do
      sort (neighborhood (100, 1200) (0, 78)) `shouldBe`
        sort
        [ (0,77)
        , (0,78), (1, 78)
        , (0,79)
        ]

    it "finds all low points in example" $ do
      lowPoints exampleInputVals `shouldBe`
        [(1,0),(9,0),(2,2),(6,4)]

    it "calculates total risk level from example" $ do
      totalLowPointRiskLevel exampleInputVals `shouldBe` 15

    it "creates 5 labels from 5 points" $ do
      countLabelled (labelPoints (10, 11) [(2, 2), (5, 2), (6, 2), (8, 3), (9, 9)])
      `shouldBe` 5

    it "finds the basin checksum in the example" $ do
      basinsChecksum exampleInputVals `shouldBe` 1134

run1 :: Input -> IO ()
run1 input = do
  let rp = totalLowPointRiskLevel input
  TextIO.putStrLn [qc|Total risk level is {rp}.|]

run2 :: Input -> IO ()
run2 input = do
  let chk = basinsChecksum input
  TextIO.putStrLn [qc|The sum of the three largest basins is {chk}.|]

main :: Task.Main
main = Task.mkParsedMain test (parser, run1) (parser, run2)
