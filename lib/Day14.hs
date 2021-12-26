module Day14
  ( main
  , test
  , spec
  ) where

import Control.Lens ( (&) )

import qualified Control.Foldl as L
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Word (Word64)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Test.Hspec.QuickCheck (prop, modifyMaxSize, modifyMaxSuccess)
import Test.QuickCheck ((===))
import Text.InterpolatedString.Perl6 (q, qc)
import Text.Megaparsec (parse, some, chunk, takeWhile1P)
import Text.Megaparsec.Char (space, letterChar)

import qualified Task
import qualified Parsers

exampleInput :: Text
exampleInput = [q|NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
|]

type Template = Seq Char

data Pair = !Char :><: !Char
  deriving (Eq, Ord, Show)

type Rules = Map Pair Char

data Instructions = Instr !Template !Rules
  deriving (Eq, Show)

exampleInputVals :: Instructions
exampleInputVals = Instr tmp rs
  where
    tmp = "NNCB"
    rs =
      Map.fromList
      [ ('C' :><: 'H', 'B')
      , ('H' :><: 'H', 'N')
      , ('C' :><: 'B', 'H')
      , ('N' :><: 'H', 'C')
      , ('H' :><: 'B', 'C')
      , ('H' :><: 'C', 'B')
      , ('H' :><: 'N', 'C')
      , ('N' :><: 'N', 'C')
      , ('B' :><: 'H', 'H')
      , ('N' :><: 'C', 'B')
      , ('N' :><: 'B', 'B')
      , ('B' :><: 'N', 'B')
      , ('B' :><: 'B', 'N')
      , ('B' :><: 'C', 'B')
      , ('C' :><: 'C', 'N')
      , ('C' :><: 'N', 'C')
      ]

parseTemplate :: Parsers.Parser Template
parseTemplate =
  Seq.fromList . Text.unpack
  <$> takeWhile1P (Just "polymer template") (not . isSpace)

parseRule :: Parsers.Parser (Pair, Char)
parseRule = do
  lhs <- letterChar
  rhs <- letterChar
  _ <- chunk " -> "
  res <- letterChar
  space
  pure (lhs :><: rhs, res)

parser :: Parsers.Parser Instructions
parser =
  Instr
  <$> parseTemplate <* space
  <*> (Map.fromList <$> some parseRule)

subst :: Rules -> Template -> Template
subst rs = go mempty . toList
  where
    go lhs = \case
      a:rest@(b:_) -> trySubst lhs a b rest
      [a]          -> lhs Seq.|> a
      []           -> lhs

    trySubst lhs a b rest =
      maybe
      (go (lhs Seq.|> a) rest)
      (\v -> go (lhs Seq.|> a Seq.|> v) rest)
      (Map.lookup (a :><: b) rs)

iterateSubst :: Rules -> Template -> [Template]
iterateSubst rs = iterate (subst rs)

countElems :: Ord a => Seq a -> Map a Word64
countElems = foldl step mempty
  where
    step acc c = Map.insertWith (+) c 1 acc

checksum :: (Foldable f, Ord a, Num a) => f a -> a
checksum cs = maxVal - minVal
  where
    (Just minVal, Just maxVal) =
      L.fold
      ((,) <$> L.minimum <*> L.maximum)
      cs

-- Wopsie, plain 'subst' is no good for 40 iterations as the Template
-- grows way too large. This structure is better suited for tracking
-- pairs as long as we don't really need the exact Template, only the
-- counts of elements in that Template.
type TemplatePairs = Map Pair Word64

checksumTemplate :: Template -> Word64
checksumTemplate = checksum . countElems

sentinel :: Char
sentinel = '_'

templatePairs :: Template -> TemplatePairs
templatePairs t = go mempty (toList padded)
    where
      -- Appended sentinel at both ends of t, so we *always* start
      -- with at least two elements. This is required for the element
      -- counting to work correctly in 'countTemplatePairElems'. It
      -- relies on all elements being counted twice, because they are
      -- present in two neighboring pairs.
      padded =
        (sentinel Seq.<| t) Seq.|> sentinel
      go tps = \case
        a:rest@(b:_) -> go (Map.insertWith (+) (a :><: b) 1 tps) rest
        _            -> tps

substPairs :: Rules -> TemplatePairs -> TemplatePairs
substPairs rs = Map.foldlWithKey' step mempty
  where
    step tps' p v =
      maybe (maybeSentinel tps' p v) (subst' tps' p v) (Map.lookup p rs)
    maybeSentinel tps' p@(a :><: b) v
      | a == sentinel || b == sentinel = Map.insert p v tps'
      | otherwise                      = tps'
    subst' tps' (a :><: c) v b =
      Map.insertWith (+) (a :><: b) v $
      Map.insertWith (+) (b :><: c) v tps'

iterateSubstPairs :: Rules -> TemplatePairs -> [TemplatePairs]
iterateSubstPairs rs = iterate (substPairs rs)

countTemplatePairElems :: TemplatePairs -> Map Char Word64
countTemplatePairElems = fixup . go
  -- template
  --   N  B  B  B  C  N  C  C  N  B  B  N  B  N  B  B  C  H  B  H  H  B  C  H  B    =>  5 N, 11B
  -- pairs
  -- _N NB BB BB BC CN NC CC CN NB BB BN NB BN NB BB BC CH HB BH HH HB BC CH HB B_  => 10 N, 22B
  -- accumulated
  --1_N4NB4BB3BC2CN1NC1CC2BN2CH3HB1BH1HH1B_                                         => 10 N, 22B
  where
    -- All elements (disregarding sentinels) got counted twice, so
    -- divide by two. Also remove sentinels; they are not really part
    -- of the template and are always just two; one at each end of the
    -- template.
    fixup countMap =
      countMap & Map.delete sentinel & fmap (`div` 2)

    go = Map.foldlWithKey' step mempty

    step countMap (a :><: b) n =
      countMap & Map.insertWith (+) a n & Map.insertWith (+) b n

checksumTemplatePairs :: TemplatePairs -> Word64
checksumTemplatePairs = checksum . countTemplatePairElems

test :: IO ()
test = Task.hspec spec

spec :: Spec
spec = do
  describe "parser" $ do

    it "parses first example input" $
      parse parser "" exampleInput `shouldParse` exampleInputVals

  describe "logic" $ do

    it "applies example rules once" $ do
      let Instr cs rs = exampleInputVals
      toList (iterateSubst rs cs !! 1) `shouldBe` "NCNBCHB"

    it "applies example rules twice" $ do
      let Instr cs rs = exampleInputVals
      toList (iterateSubst rs cs !! 2) `shouldBe` "NBCCNBBBCBHCB"

    it "applies example rules thrice" $ do
      let Instr cs rs = exampleInputVals
      toList (iterateSubst rs cs !! 3) `shouldBe` "NBBBCNCCNBBNBNBBCHBHHBCHB"

    it "finds example checksum after 10 steps" $ do
      let Instr cs rs = exampleInputVals
      checksumTemplate (iterateSubst rs cs !! 10) `shouldBe` 1588

    it "finds template pairs in example input" $ do
      let Instr cs _ = exampleInputVals
      templatePairs cs `shouldBe`
        Map.fromList
        [ ('_' :><: 'N', 1)
        , ('N' :><: 'N', 1)
        , ('N' :><: 'C', 1)
        , ('C' :><: 'B', 1)
        , ('B' :><: '_', 1)
        ]

    it "finds template pairs after one substitution round one the example input" $ do
      let Instr (templatePairs -> cs) rs = exampleInputVals
      iterateSubstPairs rs cs !! 1 `shouldBe`
        Map.fromList
        [ ('_' :><: 'N', 1)
        , ('N' :><: 'C', 1)
        , ('C' :><: 'N', 1)
        , ('N' :><: 'B', 1)
        , ('B' :><: 'C', 1)
        , ('C' :><: 'H', 1)
        , ('H' :><: 'B', 1)
        , ('B' :><: '_', 1)
        ]

    it "finds template pairs after two substitution rounds one the example input" $ do
      let Instr (templatePairs -> cs) rs = exampleInputVals
      iterateSubstPairs rs cs !! 2 `shouldBe`
        Map.fromList
        [ ('_' :><: 'N', 1)
        , ('N' :><: 'B', 2)
        , ('B' :><: 'C', 2)
        , ('C' :><: 'C', 1)
        , ('C' :><: 'N', 1)
        , ('B' :><: 'B', 2)
        , ('C' :><: 'B', 2)
        , ('B' :><: 'H', 1)
        , ('H' :><: 'C', 1)
        , ('B' :><: '_', 1)
        ]

    it "finds template pairs after three substitution rounds one the example input" $ do
      let Instr (templatePairs -> cs) rs = exampleInputVals
      iterateSubstPairs rs cs !! 3 `shouldBe`
        Map.fromList
        [ ('_' :><: 'N', 1)
        , ('N' :><: 'B', 4)
        , ('B' :><: 'B', 4)
        , ('B' :><: 'C', 3)
        , ('C' :><: 'N', 2)
        , ('N' :><: 'C', 1)
        , ('C' :><: 'C', 1)
        , ('B' :><: 'N', 2)
        , ('C' :><: 'H', 2)
        , ('H' :><: 'B', 3)
        , ('B' :><: 'H', 1)
        , ('H' :><: 'H', 1)
        , ('B' :><: '_', 1)
        ]

    it "counts template pair elements after one substitution round on the example input" $ do
      let Instr (templatePairs -> cs) rs = exampleInputVals
      countTemplatePairElems (iterateSubstPairs rs cs !! 1) `shouldBe`
        Map.fromList [ ('N', 2)
                     , ('C', 2)
                     , ('B', 2)
                     , ('H', 1)
                     ]

    it "counts template pair elements after two substitution rounds on the example input" $ do
      let Instr (templatePairs -> cs) rs = exampleInputVals
      countTemplatePairElems (iterateSubstPairs rs cs !! 2) `shouldBe`
        Map.fromList [ ('N', 2)
                     , ('B', 6)
                     , ('C', 4)
                     , ('H', 1)
                     ]

    it "counts template pair elements after three substitution rounds on the example input" $ do
      let Instr (templatePairs -> cs) rs = exampleInputVals
      countTemplatePairElems (iterateSubstPairs rs cs !! 3) `shouldBe`
        Map.fromList [ ('N', 5)
                     , ('B', 11)
                     , ('C', 5)
                     , ('H', 4)
                     ]

    -- iterateSubst quickly gets very costly on large n
    modifyMaxSize (const 14) $ modifyMaxSuccess (const 14) $
      prop "finds the same element counts after N steps using both algorithms" $
      let Instr cs rs = exampleInputVals
          tps = templatePairs cs
      in \(fromIntegral @Word @Int -> n) ->
        countTemplatePairElems (iterateSubstPairs rs tps !! n)
        ===         countElems (iterateSubst          rs  cs !! n)

    it "finds example checksum after 10 steps with second algorithm" $ do
      let Instr (templatePairs -> cs) rs = exampleInputVals
      checksumTemplatePairs (iterateSubstPairs rs cs !! 10) `shouldBe` 1588

    it "finds example checksum after 40 steps with second algorithm" $ do
      let Instr (templatePairs -> cs) rs = exampleInputVals
      checksumTemplatePairs (iterateSubstPairs rs cs !! 40) `shouldBe` 2188189693529

run1 :: Instructions -> IO ()
run1 (Instr cs rs) =
  TextIO.putStrLn [qc|The checksum is {chk} after ten steps.|]
  where
    chk = checksumTemplate (iterateSubst rs cs !! 10)

run2 :: Instructions -> IO ()
run2 (Instr (templatePairs -> cs) rs) =
  TextIO.putStrLn [qc|The checksum is {chk} after 40 steps.|]
  where
    chk = checksumTemplatePairs (iterateSubstPairs rs cs !! 40)

main :: Task.Main
main = Task.mkParsedMain test (parser, run1) (parser, run2)
