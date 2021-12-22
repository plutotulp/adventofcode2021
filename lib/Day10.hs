module Day10
  ( main
  , test
  ) where

import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Data.Word (Word64)
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.InterpolatedString.Perl6 (q, qc)
import Text.Megaparsec (label, parse, some, anySingle, try)
import Text.Megaparsec.Char (space)

import qualified Task
import qualified Parsers

exampleInput :: Text
exampleInput = [q|[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
|]

exampleInputVals :: [[Sym]]
exampleInputVals =
  [ [ Op Squ,Op Par,Op Cur,Op Par,Op Ang,Op Par,Op Par,Cl Par,Cl Par,Op Squ,Cl Squ,Cl Ang,Op Squ,Op Squ,Op Cur,Op Squ,Cl Squ,Op Cur,Op Ang,Op Par,Cl Par,Op Ang,Cl Ang,Cl Ang ]
  , [ Op Squ,Op Par,Op Par,Cl Par,Op Squ,Op Ang,Cl Ang,Cl Squ,Cl Par,Cl Squ,Op Par,Op Cur,Op Squ,Op Ang,Op Cur,Op Ang,Op Ang,Op Squ,Cl Squ,Cl Ang,Cl Ang,Op Par ]
  , [ Op Cur,Op Par,Op Squ,Op Par,Op Ang,Op Cur,Cl Cur,Op Squ,Op Ang,Cl Ang,Op Squ,Cl Squ,Cl Cur,Cl Ang,Op Cur,Op Squ,Cl Squ,Op Cur,Op Squ,Op Par,Op Ang,Op Par,Cl Par,Cl Ang ]
  , [ Op Par,Op Par,Op Par,Op Par,Op Cur,Op Ang,Cl Ang,Cl Cur,Op Ang,Op Cur,Op Ang,Op Cur,Op Ang,Cl Ang,Cl Cur,Op Cur,Op Squ,Cl Squ,Op Cur,Op Squ,Cl Squ,Op Cur,Cl Cur ]
  , [ Op Squ,Op Squ,Op Ang,Op Squ,Op Par,Op Squ,Cl Squ,Cl Par,Cl Par,Op Ang,Op Par,Op Squ,Op Squ,Op Cur,Cl Cur,Op Squ,Op Squ,Op Par,Cl Par,Cl Squ,Cl Squ,Cl Squ ]
  , [ Op Squ,Op Cur,Op Squ,Op Cur,Op Par,Op Cur,Cl Cur,Cl Squ,Op Cur,Cl Cur,Cl Cur,Op Par,Op Squ,Op Cur,Op Squ,Op Cur,Op Cur,Op Cur,Cl Cur,Cl Cur,Op Par,Op Squ,Cl Squ ]
  , [ Op Cur,Op Ang,Op Squ,Op Squ,Cl Squ,Cl Squ,Cl Ang,Cl Cur,Op Ang,Op Cur,Op Squ,Op Cur,Op Squ,Op Cur,Op Squ,Cl Squ,Op Cur,Op Par,Cl Par,Op Squ,Op Squ,Op Squ,Cl Squ ]
  , [ Op Squ,Op Ang,Op Par,Op Ang,Op Par,Op Ang,Op Par,Op Ang,Op Cur,Cl Cur,Cl Par,Cl Par,Cl Ang,Op Ang,Op Par,Op Squ,Cl Squ,Op Par,Op Squ,Cl Squ,Op Par,Cl Par ]
  , [ Op Ang,Op Cur,Op Par,Op Squ,Op Par,Op Squ,Op Squ,Op Par,Op Ang,Cl Ang,Op Par,Cl Par,Cl Par,Op Cur,Cl Cur,Cl Squ,Cl Ang,Op Par,Op Ang,Op Ang,Op Cur,Op Cur ]
  , [ Op Ang,Op Cur,Op Par,Op Squ,Op Cur,Op Cur,Cl Cur,Cl Cur,Op Squ,Op Ang,Op Squ,Op Squ,Op Squ,Op Ang,Cl Ang,Op Cur,Cl Cur,Cl Squ,Cl Squ,Cl Squ,Cl Ang,Op Squ,Cl Squ,Cl Squ ]
  ]

data BracketKind
  -- | Parenthesis
  = Par
  -- | Square bracket
  | Squ
   -- | Curly bracket
  | Cur
   -- | Angle bracket
  | Ang
  deriving (Eq, Show)

data Sym
  -- | Opening bracket
  = Op BracketKind
  -- | Closing bracket
  | Cl BracketKind
  deriving (Eq, Show)

parseLine :: Parsers.Parser [Sym]
parseLine = some (try sym) <* space
  where
    sym = do
      c <- label "bracket" anySingle
      let err = fail [qc|expected a bracket but got {c} instead|]
      maybe err pure $ case c of
        '(' -> Just (Op Par)
        ')' -> Just (Cl Par)
        '[' -> Just (Op Squ)
        ']' -> Just (Cl Squ)
        '{' -> Just (Op Cur)
        '}' -> Just (Cl Cur)
        '<' -> Just (Op Ang)
        '>' -> Just (Cl Ang)
        _   -> Nothing

parser :: Parsers.Parser [[Sym]]
parser = space *> some parseLine

data CheckResult
  = OK
  | Incomplete [BracketKind]
  | BadClosing !BracketKind !BracketKind
  | BadOpening !Sym
  deriving (Eq, Show)

check :: [Sym] -> CheckResult
check = go []
  where
    go []  [] = OK
    go stk [] = Incomplete stk
    go [] (v:vs) =
      case v of
        Op k -> go [k] vs
        Cl _ -> BadOpening v
    go stk@(s:ss) (v:vs) =
      case v of
        Op k             -> go (k : stk) vs
        Cl k | k == s    -> go ss vs
             | otherwise -> BadClosing s k

scoreCorruptedInput :: [[Sym]] -> Word64
scoreCorruptedInput = sum . fmap (score . check)
  where
    score = \case
      BadClosing _ v -> scoreClosingBracket v
      _              -> 0
    scoreClosingBracket = \case
      Par ->     3
      Squ ->    57
      Cur ->  1197
      Ang -> 25137

scoreCompletion :: [BracketKind] -> Word64
scoreCompletion = foldl step 0
  where
    step score k = (score * 5) + scoreBracketKind k
    scoreBracketKind = \case
      Par -> 1
      Squ -> 2
      Cur -> 3
      Ang -> 4

scoreInputCompletions :: [[Sym]] -> Word64
scoreInputCompletions = middle . mapMaybe (score . check)
  where
    score = \case
      Incomplete stk -> Just (scoreCompletion stk)
      _              -> Nothing
    middle ss =
      sort ss !! (length ss `div` 2)

test :: IO ()
test = Task.hspec $ do

  describe "parser" $ do

    it "parses example input" $
      parse parser "" exampleInput `shouldParse` exampleInputVals

  describe "logic" $ do

    it "verifies that {([(<{}[<>[]}>{[]{[(<()> is corrupt" $ do
      -- pattern match assumes parser succeeds
      let Right v = parse parseLine "" "{([(<{}[<>[]}>{[]{[(<()>"
      check v `shouldBe` BadClosing Squ Cur

    it "checks all rows in the example input" $ do
      check <$> exampleInputVals
        `shouldBe`
        [ Incomplete [Cur,Cur,Squ,Squ,Par,Cur,Par,Squ]
        , Incomplete [Par,Cur,Ang,Squ,Cur,Par]
        , BadClosing Squ Cur
        , Incomplete [Cur,Cur,Ang,Cur,Ang,Par,Par,Par,Par]
        , BadClosing Squ Par
        , BadClosing Par   Squ
        , Incomplete [Squ,Squ,Cur,Cur,Squ,Cur,Squ,Cur,Ang]
        , BadClosing Ang  Par
        , BadClosing Squ Ang
        , Incomplete [Squ,Par,Cur,Ang]
        ]

    it "scores the example corrupted input" $ do
      scoreCorruptedInput exampleInputVals `shouldBe` 26397

    it "scores the short completion example" $ do
      scoreCompletion [Squ, Par, Cur, Ang] `shouldBe` 294

    it "finds the middle score from the example input completions" $ do
      scoreInputCompletions exampleInputVals `shouldBe` 288957

run1 :: [[Sym]] -> IO ()
run1 input =
  TextIO.putStrLn [qc|Final score is {scoreCorruptedInput input}.|]

run2 :: [[Sym]] -> IO ()
run2 input =
  TextIO.putStrLn [qc|Completions middle score is {scoreInputCompletions input}|]

main :: Task.Main
main = Task.mkParsedMain test (parser, run1) (parser, run2)
