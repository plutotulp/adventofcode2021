module Day08
  ( main
  , test
  ) where

import Control.Lens

import Control.Monad.ST (runST)
import Data.Bits ((.|.), bit, popCount, testBit)
import Data.Generics.Labels ()
import Data.List (permutations)
import Data.Monoid (Sum(Sum, getSum))
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified  Data.Vector as V
import qualified  Data.Vector.Generic as VG
import qualified  Data.Vector.Generic.Mutable as VGM
import qualified  Data.Vector.Unboxed as VU
import Data.Word (Word8, Word64)
import GHC.Generics (Generic)
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.InterpolatedString.Perl6 (q, qc)
import Text.Megaparsec ((<|>), parse, some)
import Text.Megaparsec.Char (char, space, hspace)
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Task
import qualified Parsers

exampleInput :: Text
exampleInput = [q|be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce|]

-- The active bit for each Segment pattern defined below.
type SegmentBit = Int
pattern BitA, BitB, BitC, BitD, BitE, BitF, BitG :: SegmentBit
pattern BitA = 0
pattern BitB = 1
pattern BitC = 2
pattern BitD = 3
pattern BitE = 4
pattern BitF = 5
pattern BitG = 6

-- Ordered segment bits, from segment A to G.
allSegmentBits :: VU.Vector SegmentBit
allSegmentBits = [BitA, BitB, BitC, BitD, BitE, BitF, BitG]

type Segment = Word8
pattern A, B, C, D, E, F, G :: Segment
pattern A = 0b00000001
pattern B = 0b00000010
pattern C = 0b00000100
pattern D = 0b00001000
pattern E = 0b00010000
pattern F = 0b00100000
pattern G = 0b01000000

type Digit = Word8

-- A digit is a bitwise or of all its segments.
mkDigit :: VU.Vector Segment -> Digit
mkDigit  = VG.foldl (.|.) 0

data Entry =
  Entry
  { patterns :: !(VU.Vector Digit)
  , digits   :: !(VU.Vector Digit)
  }
  deriving stock (Eq, Show, Generic)

exampleInputVals :: V.Vector Entry
exampleInputVals =
  [ Entry { patterns = [mkDigit [B,E],mkDigit [A,B,C,D,E,F,G],mkDigit [B,C,D,E,F,G],mkDigit [A,C,D,E,F,G],mkDigit [B,C,E,G],mkDigit [C,D,E,F,G],mkDigit [A,B,D,E,F,G],mkDigit [B,C,D,E,F],mkDigit [A,B,C,D,F],mkDigit [B,D,E]]
          , digits = [mkDigit [A,B,C,D,E,F,G],mkDigit [B,C,D,E,F],mkDigit [B,C,D,E,F,G],mkDigit [B,C,E,G]]}
  , Entry { patterns = [mkDigit [A,B,D,E,F,G],mkDigit [B,C,D,E,G],mkDigit [B,C,G],mkDigit [C,G],mkDigit [A,B,C,D,E,F,G],mkDigit [B,D,E,F,G],mkDigit [A,B,C,D,F,G],mkDigit [A,B,C,D,E],mkDigit [B,C,D,E,F,G],mkDigit [C,E,F,G]]
          , digits = [mkDigit [B,C,D,E,F,G],mkDigit [B,C,G],mkDigit [A,B,C,D,E,F,G],mkDigit [C,G]]}
  , Entry { patterns = [mkDigit [A,B,D,E,F,G],mkDigit [C,G],mkDigit [A,B,C,D,E],mkDigit [A,B,D,F,G],mkDigit [A,B,C,D,F,G],mkDigit [B,C,D,E,F,G],mkDigit [A,B,C,D,G],mkDigit [A,C,F,G],mkDigit [B,C,G],mkDigit [A,B,C,D,E,F,G]]
          , digits = [mkDigit [C,G],mkDigit [C,G],mkDigit [A,B,C,D,F,G],mkDigit [B,C,G]]}
  , Entry { patterns = [mkDigit [B,C,D,E,F,G],mkDigit [B,C,D],mkDigit [A,B,C,D,E,F],mkDigit [A,B,D,E,G],mkDigit [A,B,C,F],mkDigit [B,C],mkDigit [A,C,D,E,F],mkDigit [A,B,C,D,E],mkDigit [A,C,D,E,F,G],mkDigit [A,B,C,D,E,F,G]]
          , digits = [mkDigit [A,B,C,D,E,F],mkDigit [A,B,C,D,E],mkDigit [A,C,D,E,F,G],mkDigit [B,C]]}
  , Entry { patterns = [mkDigit [A,B,C,D,E,F,G],mkDigit [B,F,G],mkDigit [F,G],mkDigit [A,B,E,F,G],mkDigit [A,B,D,E,F],mkDigit [C,E,F,G],mkDigit [A,B,C,E,G],mkDigit [A,B,C,E,F,G],mkDigit [A,B,C,D,E,G],mkDigit [A,B,C,D,F,G]]
          , digits = [mkDigit [C,E,F,G],mkDigit [A,B,C,D,E,F,G],mkDigit [B,F,G],mkDigit [A,B,E,F,G]]}
  , Entry { patterns = [mkDigit [A,B,E,F,G],mkDigit [A,C],mkDigit [A,B,C,E,F,G],mkDigit [A,B,C,D,E,F,G],mkDigit [A,C,D,E,F,G],mkDigit [B,C,D,F,G],mkDigit [A,B,C,E],mkDigit [A,B,D,E,F,G],mkDigit [A,B,C,F,G],mkDigit [A,C,F]]
          , digits = [mkDigit [A,B,C,D,E,F,G],mkDigit [A,B,C,E],mkDigit [A,C],mkDigit [A,B,C,D,E,F,G]]}
  , Entry { patterns = [mkDigit [B,C,D,F,G],mkDigit [D,F,G],mkDigit [A,B,C,D,E,F,G],mkDigit [C,E,F,G],mkDigit [A,B,D,E,F,G],mkDigit [A,B,C,D,E,F],mkDigit [B,C,D,E,F],mkDigit [A,B,C,D,G],mkDigit [B,C,D,E,F,G],mkDigit [F,G]]
          , digits = [mkDigit [C,E,F,G],mkDigit [B,C,D,E,F],mkDigit [C,E,F,G],mkDigit [A,B,C,D,E,F,G]]}
  , Entry { patterns = [mkDigit [B,C,D,E,F,G],mkDigit [A,B,C,E,F,G],mkDigit [B,C,E,F,G],mkDigit [A,C,D,E,F,G],mkDigit [A,B,C,D,G],mkDigit [D,E],mkDigit [B,D,E,F],mkDigit [C,D,E],mkDigit [A,B,C,D,E,F,G],mkDigit [B,C,D,E,G]]
          , digits = [mkDigit [D,E],mkDigit [A,B,C,E,F,G],mkDigit [A,B,C,D,G],mkDigit [B,C,E,F,G]]}
  , Entry { patterns = [mkDigit [A,B,D,E,F,G],mkDigit [B,C,D,E,F,G],mkDigit [C,D,E,G],mkDigit [A,B,C,E,F],mkDigit [B,C,G],mkDigit [A,B,C,D,E,F,G],mkDigit [C,G],mkDigit [A,B,C,D,F,G],mkDigit [B,D,E,F,G],mkDigit [B,C,E,F,G]]
          , digits = [mkDigit [A,B,C,D,E,F,G],mkDigit [B,C,G],mkDigit [C,G],mkDigit [B,C,G]]}
  , Entry { patterns = [mkDigit [A,B,C,F,G],mkDigit [C,F,G],mkDigit [A,B,C,D,E,F,G],mkDigit [A,B,C,E,G],mkDigit [F,G],mkDigit [A,B,C,D,E,G],mkDigit [A,E,F,G],mkDigit [A,B,C,E,F,G],mkDigit [A,B,C,D,F],mkDigit [B,C,D,E,F,G]]
          , digits = [mkDigit [A,E,F,G],mkDigit [A,B,C,F,G],mkDigit [F,G],mkDigit [A,B,C,E,G]]}]

parseSegment :: Parsers.Parser Segment
parseSegment =
  (char 'a' *> pure A) <|>
  (char 'b' *> pure B) <|>
  (char 'c' *> pure C) <|>
  (char 'd' *> pure D) <|>
  (char 'e' *> pure E) <|>
  (char 'f' *> pure F) <|>
  (char 'g' *> pure G)

parseDisplayDigit :: Parsers.Parser Digit
parseDisplayDigit = mkDigit . VU.fromList <$> some parseSegment <* hspace

parseEntry :: Parsers.Parser Entry
parseEntry = do
  ps <- VG.replicateM 10 parseDisplayDigit
  L.symbol hspace "|"
  ds  <- VG.replicateM  4 parseDisplayDigit
  space
  pure $ Entry { patterns = ps, digits = ds }

parser :: Parsers.Parser (V.Vector Entry)
parser = VG.fromList <$> (space *> some parseEntry)

-- val     0  1  2  3  4  5  6  7  8  9
-- nsegs   6  2  5  5  4  5  6  3  7  6
-- unique     x        x        x  x
countUniqueDigits :: Entry -> Int
countUniqueDigits e = getSum (VG.foldMap f (e ^. #digits))
  where
    f (popCount -> n) | n == 2 || n == 4 || n == 3 || n == 7 = Sum 1
                      | otherwise                            = Sum 0

-- Ordered digits, from 0 to 9.
allDigits :: VU.Vector Digit
allDigits = VG.fromListN 10 ds
  where
    ds = [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9]
    d0 = mkDigit [A, B, C,    E, F, G]
    d1 = mkDigit [C,             F   ]
    d2 = mkDigit [A,    C, D, E,    G]
    d3 = mkDigit [A,    C, D,    F, G]
    d4 = mkDigit [   B, C, D,    F   ]
    d5 = mkDigit [A, B,    D,    F, G]
    d6 = mkDigit [A, B,    D, E, F, G]
    d7 = mkDigit [A,    C,       F   ]
    d8 = mkDigit [A, B, C, D, E, F, G]
    d9 = mkDigit [A, B, C, D,    F, G]

-- Convert signal digit into number 0-9, or Nothing if the pattern is
-- unknown.
renderDigit :: Digit -> Maybe Word8
renderDigit d = fromIntegral <$> VG.findIndex (== d) allDigits

-- Input is signal digit, output is the display output from hooking up
-- wires in one configuration.
type Wires = Digit -> Digit

-- Convert signal digit into shown number using the provided wiring,
-- or Nothing if the digit signal + wiring produces garbled output.
renderWiredDigit :: Wires -> Digit -> Maybe Word8
renderWiredDigit wires = renderDigit . wires

renderWired :: Wires -> VU.Vector Digit -> Maybe [Word8]
renderWired wires segs =
  sequence $ (renderWiredDigit wires <$> VG.toList segs)

-- >>> toDecimal [4,2,3,1]
-- 4231
toDecimal :: [Word8] -> Word64
toDecimal = getSum . foldMap mul . zip pows . reverse
  where
    mul (p, d) = Sum (p * fromIntegral d)

-- >>> take 4 pows
-- [0, 10, 100, 1000]
pows :: [Word64]
pows = map (10^) (enumFrom @Word64 0)

-- Wire constructor. Input is list of seven (signal, output) bit mappings,
-- one for each segment on the display.
mkWires :: [(SegmentBit, SegmentBit)] -> Wires
mkWires cfg = go
  where
    nseg = VG.length allSegmentBits
    go d = mkDigit $ VG.map (tr d) allSegmentBits
    tr d i | testBit d i = bit (mp VG.! i)
           | otherwise   = 0
    -- Mapping vector. Entry at signal bit i holds corresponding
    -- output bit.
    mp = runST @(VU.Vector SegmentBit) $ do
      vec <- VGM.replicate nseg 0
      mapM_ (uncurry (VGM.write vec)) cfg
      VG.freeze vec

example1 :: (Wires, Entry)
example1 = (wires, entry)
  where
    wires =
      mkWires
      [ (BitD, BitA)
      , (BitE, BitB)
      , (BitA, BitC)
      , (BitF, BitD)
      , (BitG, BitE)
      , (BitB, BitF)
      , (BitC, BitG)
      ]
    entry =
      either (error . show) id $
      parse parseEntry "" "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |cdfeb fcadb cdfeb cdbaf"

-- All 5040 possible wirings.
allWires :: V.Vector Wires
allWires =
  VG.fromList $ (mkWires' <$> permutations (VG.toList allSegmentBits))
  where
    mkWires' :: [SegmentBit] -> Wires
    mkWires' cfg = mkWires (zip cfg (VG.toList allSegmentBits))

-- Predicate: Does wiring produce readable numbers in all patterns and
-- digits of the entry?
validWires :: Entry -> Wires -> Bool
validWires entry wires =
  validPatterns && validDigits
  where
    x `hasN` n    = maybe False ((== n) . length) x
    rendered      = renderWired wires . flip view entry
    validPatterns = rendered #patterns `hasN` 10
    validDigits   = rendered #digits   `hasN`  4

-- Brute force search for valid wiring. Expecting to find just a
-- single valid wiring, so finding more than one yields Nothing.
findWires :: Entry -> Maybe Wires
findWires entry =
  case VG.filter (validWires entry) allWires of
    [wires] -> Just wires
    _       -> Nothing

solveEntry :: Entry -> Maybe Word64
solveEntry entry = do
  wires <- findWires entry
  toDecimal <$> renderWired wires (entry ^. #digits)

solveAllEntries :: V.Vector Entry -> Maybe Word64
solveAllEntries = fmap VG.sum . VG.sequence . VG.map solveEntry

test :: IO ()
test = Task.hspec $ do

  describe "parser" $ do

    it "parses example input" $
      parse parser "" exampleInput `shouldParse` exampleInputVals

  describe "logic" $ do

    it "counts unique digits in example" $ do
      getSum (VG.foldMap (Sum . countUniqueDigits) exampleInputVals)
        `shouldBe` 26

    it "correctly wires up example 1" $
      let (wires, entry) = example1
      in renderWired wires (entry ^. #digits) `shouldBe` Just [5, 3, 5, 3]

    it "finds example wiring" $ do
      let entry = snd example1
          ds = entry ^. #digits
          ws = findWires entry
      flip renderWired ds <$> ws `shouldBe` (Just (Just [5,3,5,3]))

    it "converts 10 to decimal" $ do
      toDecimal [1,0] `shouldBe` 10

    it "converts 8251 to decimal" $ do
      toDecimal [8,2,5,1] `shouldBe` 8251

    it "solves all entries in example" $ do
      solveAllEntries exampleInputVals
      `shouldBe` Just 61229

run1 :: V.Vector Entry -> IO ()
run1 es = do
  let n = getSum (VG.foldMap (Sum . countUniqueDigits) es)
  TextIO.putStrLn [qc|Found {n} unique digits in input.|]

run2 :: V.Vector Entry -> IO ()
run2 es = do
  let nes = VG.length es
      res = solveAllEntries es
  case res of
    Nothing -> TextIO.putStrLn [qc|Failed to solve all entries.|]
    Just n  -> TextIO.putStrLn [qc|The final checksum of all {nes} entries is {n}.|]

main :: Task.Main
main = Task.mkParsedMain test (parser, run1) (parser, run2)
