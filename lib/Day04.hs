module Day04
  ( main
  , test
  , spec
  ) where

import Control.Lens

import Data.Bits ((.&.), setBit, testBit, shiftL, shiftR)
import Data.Generics.Labels ()
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32)
import GHC.Generics (Generic)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.InterpolatedString.Perl6 (q,qc)
import Text.Megaparsec (some, parse, count, sepBy1)
import Text.Megaparsec.Char (space, hspace)
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Task
import qualified Parsers

exampleInput :: Text
exampleInput =
  [q|7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7|]

type Number = Word
type Marks = Word32

data Board =
  Board
  { vals  :: !(VU.Vector Number)
  , marks :: !Marks
  }
  deriving (Eq, Generic, Show)

board :: Iso' (VU.Vector Word, Marks) Board
board = iso f g
  where
    f (v, m) = Board v m
    g b = (b ^. #vals, b ^. #marks)

type Input = ([Number], V.Vector Board)

unmarked :: Marks
unmarked = 0

offset :: Num a => a -> a -> a
offset row col = row * 5 + col

markAt :: Marks -> Int -> Int -> Marks
markAt marks' row col = mark marks' (offset row col)

mark :: Marks -> Int -> Marks
mark = setBit

bingo :: Marks -> Bool
bingo marks' = VG.or checked
  where
    checked = VG.zipWith (==) masked bingoes
    masked = VG.map (marks' .&.) bingoes
    r1 = 0b0000000000000000000011111
    r2 = shiftL r1 5
    r3 = shiftL r1 10
    r4 = shiftL r1 15
    r5 = shiftL r1 20
    c1 = 0b1000010000100001000010000
    c2 = shiftR c1 1
    c3 = shiftR c1 2
    c4 = shiftR c1 3
    c5 = shiftR c1 4
    bingoes = VU.fromList [ r1, r2, r3, r4, r5, c1, c2, c3, c4, c5 ]

exampleInputVals :: Input
exampleInputVals = (drawn, boards)
  where
    drawn = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
    board1 = (VG.fromList [22, 13, 17, 11, 0, 8, 2, 23, 4, 24, 21, 9, 14, 16, 7, 6, 10, 3, 18, 5, 1, 12, 20, 15, 19], unmarked) ^. board
    board2 = (VG.fromList [3, 15,  0,  2, 22, 9, 18, 13, 17,  5, 19,  8,  7, 25, 23, 20, 11, 10, 24, 4, 14, 21, 16, 12, 6], unmarked) ^. board
    board3 = (VG.fromList [14, 21, 17, 24,  4, 10, 16, 15,  9, 19, 18,  8, 23, 26, 20, 22, 11, 13,  6,  5, 2,  0, 12,  3,  7], unmarked) ^. board
    boards = VG.fromList [board1, board2, board3]

-- Has bingo in row 0
exampleBingoMarks :: Marks
exampleBingoMarks =
  foldl @[] step 0
  [ (0, 0)
  , (0, 1)
  , (0, 2)
  , (0, 3)
  , (0, 4)
  , (1, 3)
  , (2, 2)
  , (3, 1)
  , (4, 0)
  , (4, 1)
  , (4, 4)
  ]
  where
    step w (r, c) = markAt w r c

-- Does not have bingo
exampleNotBingoMarks :: Marks
exampleNotBingoMarks =
  foldl @[] step 0
  [ (0, 0)
  , (0, 1)
  , (0, 2)
  -- removed (0, 3) from exampleBingoMarks
  , (0, 4)
  , (1, 3)
  , (2, 2)
  , (3, 1)
  , (4, 0)
  , (4, 1)
  , (4, 4)
  ]
  where
    step w (r, c) = markAt w r c

parser :: Parsers.Parser Input
parser = do
  space
  ds <- sepBy1 drawnNum (L.symbol hspace ",")
  space
  bs <- some board'
  pure (ds, VG.fromList bs)
  where
    board' = do
      vals' <- count 25 boardNum
      pure $ (VG.fromList vals', unmarked) ^. board
    drawnNum = L.lexeme hspace L.decimal
    boardNum = L.lexeme space L.decimal

markBoard :: Board -> Number-> Board
markBoard board' number = maybe board' mark' lookupNum
  where
    lookupNum = board' ^. #vals . to (VG.findIndex (== number))
    mark' offset' = board' & #marks %~ flip mark offset'

markBoardsVector :: V.Vector Board -> Number -> V.Vector Board
markBoardsVector boards number = V.map (`markBoard` number) boards

boardHasBingo :: Board -> Bool
boardHasBingo = bingo . view #marks

-- | Produces list of winners in all rounds that produce winners. Each
-- set of results includes the list of drawn Numbers that produced the
-- winners.
--
-- Note that this algorithm assumes that boards go out of play when
-- they get a bingo.
fullGame ::
  Input ->
  [(V.Vector (Board, Score), [Number])]
fullGame (numbers, boards0) =
  go [] boards0 numbers
  where
    go _    _ []               = []
    go _  bs0 _  | VG.null bs0 = []
    go seen0 bs0 (n:ns) =
      let scoredWinners = VG.map (\b -> (b, score n b)) ws
          seen = seen0 ++ [n]
          -- bs are boards remaining in play after winners have been
          -- moved into ws.
          (ws, bs) = VG.partition boardHasBingo (markBoardsVector bs0 n)
          noWinners = VG.null ws
          next = go seen bs ns
      in if noWinners then next else (scoredWinners, seen) : next

newtype Score = Score Word32
  deriving (Eq, Num, Show) via Word32

score :: Number -> Board -> Score
score lastDraw board' = sum unmarked' * fromIntegral lastDraw
  where
    marks' = board' ^. #marks
    notMarked = not . testBit marks' . fst
    indexedVals = zip [0..] (VG.toList (board' ^. #vals))
    unmarked' = fromIntegral . snd <$> filter notMarked indexedVals

test :: IO ()
test = Task.hspec spec

spec :: Spec
spec = do

  describe "parser" $ do

    it "parses example input" $
      parse parser "" exampleInput `shouldParse` exampleInputVals

  describe "logic" $ do

    it "marks bingo example" $ do
      bingo exampleBingoMarks `shouldBe` True

    it "does not mark a non-bingo example as bingo" $ do
      bingo exampleNotBingoMarks `shouldBe` False

    it "finds first example bingo" $ do
      let (VG.head -> (_, score'), last -> lastDraw) =
            head (fullGame exampleInputVals)
      (lastDraw, score') `shouldBe` (24, 4512)

    it "finds last example bingo" $ do
      let (VG.head -> (_, score'), last -> lastDraw) =
            last (fullGame exampleInputVals)
      (lastDraw, score') `shouldBe` (13, 1924)

run1 :: Input -> IO ()
run1 input =
  maybe notFound found (nonEmpty (fullGame input))
  where
    notFound =
      TextIO.putStrLn "Did not find any winners."
    found (NonEmpty.head -> (boardsAndScores, _)) =
      VG.forM_ boardsAndScores $ \(_, score') -> do
        TextIO.putStrLn [qc|Winner board score: {score'}|]

run2 :: Input -> IO ()
run2 input =
  maybe notFound found (nonEmpty (fullGame input))
  where
    notFound =
      TextIO.putStrLn "Did not find any winners."
    found (NonEmpty.last -> (boardsAndScores, _)) =
      VG.forM_ boardsAndScores $ \(_, score') -> do
        TextIO.putStrLn [qc|Last winner board score: {score'}|]

main :: Task.Main
main = Task.mkParsedMain test (parser, run1) (parser, run2)
