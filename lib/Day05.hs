module Day05
  ( main
  , test
  ) where

import Control.Lens

import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Data.Word (Word32)
import GHC.Generics (Generic)
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.InterpolatedString.Perl6 (q, qc)
import Text.Megaparsec (some, parse)
import Text.Megaparsec.Char (space, hspace)
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Task
import qualified Parsers

exampleInput :: Text
exampleInput =
  [q|0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2|]

data Coord =
  Coord
  { x :: !Word
  , y :: !Word
  }
  deriving stock (Eq, Generic, Ord, Show)

data Segment =
  Segment
  { c1 :: !Coord
  , c2 :: !Coord
  }
  deriving stock (Eq, Generic, Show)

exampleInputVals :: [Segment]
exampleInputVals =
  [ Segment (Coord 0 9) (Coord 5 9)
  , Segment (Coord 8 0) (Coord 0 8)
  , Segment (Coord 9 4) (Coord 3 4)
  , Segment (Coord 2 2) (Coord 2 1)
  , Segment (Coord 7 0) (Coord 7 4)
  , Segment (Coord 6 4) (Coord 2 0)
  , Segment (Coord 0 9) (Coord 2 9)
  , Segment (Coord 3 4) (Coord 1 4)
  , Segment (Coord 0 0) (Coord 8 8)
  , Segment (Coord 5 5) (Coord 8 2)
  ]


parseCoord :: Parsers.Parser Coord
parseCoord =
  (Coord <$> (decimal <* comma) <*> decimal) <* hspace
  where
    decimal = L.lexeme hspace L.decimal
    comma   = L.symbol hspace ","

parseSegment :: Parsers.Parser Segment
parseSegment =
  (Segment <$> (parseCoord <* arrow) <*> parseCoord) <* space
  where
    arrow = L.symbol hspace "->"

parser :: Parsers.Parser [Segment]
parser = space *> some parseSegment

isVertical :: Segment -> Bool
isVertical seg =
  seg ^. #c1 . #x == seg ^. #c2 . #x

isHorisontal :: Segment -> Bool
isHorisontal seg =
  seg ^. #c1 . #y == seg ^. #c2 . #y

rangeHorizOrVert :: Segment -> [Coord]
rangeHorizOrVert seg
  | isHorisontal seg =
      let y' = seg ^. #c1 . #y
          mkCoord x' = Coord x' y'
          x0 = min (seg ^. #c1 . #x) (seg ^. #c2 . #x)
          x1 = max (seg ^. #c1 . #x) (seg ^. #c2 . #x)
      in mkCoord <$> [x0..x1]
  | isVertical seg =
      let x' = seg ^. #c1 . #x
          mkCoord y' = Coord x' y'
          y0 = min (seg ^. #c1 . #y) (seg ^. #c2 . #y)
          y1 = max (seg ^. #c1 . #y) (seg ^. #c2 . #y)
      in mkCoord <$> [y0..y1]
  | otherwise = []

-- Enumerates [a..b], going upwards or downwards as needed.
fromTo :: (Enum a, Integral a, Ord a) => a -> a -> [a]
fromTo a b = if a <= b then upwards else downwards
  where
    upwards   = take d $ enumFromThen a (succ a)
    downwards = take d $ enumFromThen a (pred a)
    d = 1 + abs (fromIntegral a - fromIntegral b)

range :: Segment -> [Coord]
range seg = cs
  where
    cs | length xs == length ys = zipWith mkCoord xs ys
       | otherwise = error "not diagonal, horizontal or vertical"
    mkCoord x' y' = Coord x' y'
    xs = case fromTo (seg ^. #c1 . #x) (seg ^. #c2 . #x) of
           [x'] -> replicate (length ys) x'
           xs' -> xs'
    ys = case fromTo (seg ^. #c1 . #y) (seg ^. #c2 . #y) of
           [y'] -> replicate (length xs) y'
           ys' -> ys'

countCoords :: [Coord] -> Map.Map Coord Word32
countCoords = Map.fromListWith (+) . fmap (, 1)

numDangerous :: (Num c, Foldable t) => (a -> [Coord]) -> t a -> c
numDangerous rangeFun =
  Map.foldr step 0 . countCoords . concatMap rangeFun
  where
    step overlaps count | 1 < overlaps = count + 1
                        | otherwise    = count

numDangerousHorizOrVertLines :: [Segment] -> Word32
numDangerousHorizOrVertLines = numDangerous rangeHorizOrVert

numDangerousHorizVerOrDiagLines :: [Segment] -> Word32
numDangerousHorizVerOrDiagLines = numDangerous range

test :: IO ()
test = Task.hspec $ do

  describe "parser" $ do

    it "parses example input" $ do
      parse parser "" exampleInput `shouldParse` exampleInputVals

  describe "logic" $ do

    it "counts dangerous areas in first example" $ do
      numDangerousHorizOrVertLines exampleInputVals `shouldBe` 5

    it "ranges a diagonal" $ do
      range (Segment (Coord 0 0) (Coord 4 4)) `shouldBe`
        [Coord 0 0, Coord 1 1, Coord 2 2, Coord 3 3, Coord 4 4]

    it "ranges a second diagonal" $ do
      range (Segment (Coord 3 3) (Coord 0 0)) `shouldBe`
        [Coord 3 3, Coord 2 2, Coord 1 1, Coord 0 0]

    it "counts dangerous areas in second example" $ do
      numDangerousHorizVerOrDiagLines exampleInputVals `shouldBe` 12

run1 :: [Segment] -> IO ()
run1 segs = do
  let n = numDangerousHorizOrVertLines segs
  TextIO.putStrLn [qc|Found {n} dangerous areas.|]

run2 :: [Segment] -> IO ()
run2 segs = do
  let n = numDangerousHorizVerOrDiagLines segs
  TextIO.putStrLn [qc|Found {n} dangerous areas.|]

main :: Task.Main
main = Task.mkParsedMain test (parser, run1) (parser, run2)
