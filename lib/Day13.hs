module Day13
  ( main
  , test
  ) where

import qualified Control.Foldl as L
import Control.Monad.ST (runST)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Profunctor (lmap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.InterpolatedString.Perl6 (q, qc)
import Text.Megaparsec ((<|>), parse, some, single, chunk)
import Text.Megaparsec.Char (space, hspace)
import Text.Megaparsec.Char.Lexer (decimal)

import qualified Task
import qualified Parsers

exampleInput :: Text
exampleInput = [q|6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
|]

data Point = Pt !Int !Int
  deriving (Eq, Ord, Show)

getX, getY :: Point -> Int
getX (Pt x _) = x
getY (Pt _ y) = y

data Dir
  = AlongY
  | AlongX
  deriving (Eq, Show)

data FoldLine = FoldLine !Dir !Int
  deriving (Eq, Show)

data Instructions =
  Instructions
  { points :: Set Point
  , folds :: [FoldLine]
  }
  deriving (Eq, Show)

exampleInputVals :: Instructions
exampleInputVals =
  Instructions
  { points = Set.fromList [Pt 6 10, Pt 0 14, Pt 9 10, Pt 0 3, Pt 10 4, Pt 4 11, Pt 6 0, Pt 6 12, Pt 4 1, Pt 0 13, Pt 10 12, Pt 3 4, Pt 3 0, Pt 8 4, Pt 1 10, Pt 2 14, Pt 8 10, Pt 9 0]
  , folds  = [FoldLine AlongY 7, FoldLine AlongX 5]
  }

parseDir :: Parsers.Parser Dir
parseDir = (single 'y' $> AlongY) <|> (single 'x' $> AlongX)

parseFoldLine :: Parsers.Parser FoldLine
parseFoldLine = do
  _     <- chunk "fold along"
  hspace
  dir   <- parseDir
  _     <- single '='
  digit <- decimal
  space
  pure $ FoldLine dir digit

parsePoint :: Parsers.Parser Point
parsePoint = do
  x' <- decimal
  _  <- single ','
  y' <- decimal
  space
  pure $ Pt x' y'

parser :: Parsers.Parser Instructions
parser = do
  ps <- Set.fromList <$> some parsePoint
  fs <- some parseFoldLine
  pure $ Instructions { points = ps, folds = fs }

-- | Follow the folding instructions, step by step. The first result
-- is from before folding, the second result is after folding once,
-- and so on until there are no more folding instructions.
scanFoldLines :: Instructions -> [Set Point]
scanFoldLines instr = scanl step (points instr) (folds instr)
  where
    step pts = \case
      FoldLine AlongX n -> Set.map (foldX n) pts
      FoldLine AlongY n -> Set.map (foldY n) pts

-- | @fld n p@ folds along line at n and yields the new value for
-- position p.
fld :: Int -> Int -> Int
fld n p | n <= p    = 2 * n - p
        | otherwise = p

foldX, foldY :: Int -> Point -> Point
foldX n (Pt x y) = Pt (fld n x) y
foldY n (Pt x y) = Pt x (fld n y)

renderBox :: Int -> VU.Vector Char -> Text
renderBox nx can = Text.unlines (Text.pack . VG.toList <$> chunkVec nx can)

chunkVec :: VG.Vector v a => Int -> v a -> [v a]
chunkVec n cs0 | VG.null cs0 = []
               | otherwise   = let (c, cs) = VG.splitAt n cs0
                               in c : chunkVec n cs

renderPoints :: Set Point -> Text
renderPoints pts | Set.null pts = ""
                 | otherwise    = renderBox nx box
  where
    (nx, ny) = (1 + maxX - minX, 1 + maxY - minY)
    box = runST $ do
      vec <- VGM.replicate (nx * ny) ' '
      for_ (Set.toList pts) $ \(Pt x y) -> VGM.write vec (x + y * nx) '#'
      VG.unsafeFreeze vec
    -- Assumes non-empty pts
    (Just minX, Just minY, Just maxX, Just maxY) =
      L.fold
      ((,,,)
       <$> lmap getX L.minimum
       <*> lmap getY L.minimum
       <*> lmap getX L.maximum
       <*> lmap getY L.maximum)
      (Set.toList pts)

test :: IO ()
test = Task.hspec $ do

  describe "parser" $ do

    it "parses first example input" $
      parse parser "" exampleInput `shouldParse` exampleInputVals

  describe "logic" $ do

    it "counts the number of dots after folding the example once" $ do
      Set.size (scanFoldLines exampleInputVals !! 1) `shouldBe` 17

run1 :: Instructions -> IO ()
run1 instr =
  TextIO.putStrLn [qc|There are {n} visible points after folding once.|]
  where
    n = Set.size (scanFoldLines instr !! 1)

run2 :: Instructions -> IO ()
run2 instr =
  TextIO.putStrLn (renderPoints (last (scanFoldLines instr)))

main :: Task.Main
main = Task.mkParsedMain test (parser, run1) (parser, run2)
