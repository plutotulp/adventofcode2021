module Day02
  ( main
  , test
  ) where

import Control.Lens

import Control.Monad.Trans.State (execState)
import Data.Foldable (traverse_)
import Data.Generics.Labels ()
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import GHC.Generics (Generic)
import Test.Hspec (describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.InterpolatedString.Perl6 (q, qc)
import Text.Megaparsec (some, parse, (<|>))
import Text.Megaparsec.Char (space)

import qualified Task
import qualified Parsers

exampleInput :: Text
exampleInput =
  [q|forward 5
down 5
forward 8
up 3
down 8
forward 2|]

data NavCmd = Forward Int | Down Int | Up Int
  deriving stock (Eq, Show)

type Course = [NavCmd]

exampleInputVals :: Course
exampleInputVals =
  [ Forward 5
  , Down 5
  , Forward 8
  , Up 3
  , Down 8
  , Forward 2
  ]

parseNavCmd :: Parsers.Parser NavCmd
parseNavCmd = fwd' <|> down' <|> up'
  where
    p txt = Parsers.symbol txt *> Parsers.decimal
    fwd'  = Forward <$> p "forward"
    down' = Down    <$> p "down"
    up'   = Up      <$> p "up"

parser :: Parsers.Parser [NavCmd]
parser = space *> some parseNavCmd

data Loc =
  Loc
  { depth :: !Int
  , horizontalPos :: !Int
  , aim :: !Int
  }
  deriving (Generic, Show)

startLoc :: Loc
startLoc = Loc { depth = 0, horizontalPos = 0, aim = 0 }

followCourse1 :: Loc -> Course -> Loc
followCourse1 loc0 ncs = execState (traverse_ step ncs) loc0
  where
    step = \case
      Down    n -> #depth += n
      Up      n -> #depth -= n
      Forward n -> #horizontalPos += n

followCourse2 :: Loc -> Course -> Loc
followCourse2 loc0 ncs = execState (traverse_ step ncs) loc0
  where
    step = \case
      Down    n -> #aim += n
      Up      n -> #aim -= n
      Forward n -> do #horizontalPos += n
                      a <- use #aim
                      #depth += a * n

checksum :: Loc -> Int
checksum loc = loc ^. #depth * loc ^. #horizontalPos

test :: IO ()
test = Task.hspec $ do
  describe "parser" $ do
    it "parses example input" $
      parse parser "" exampleInput `shouldParse` exampleInputVals
  describe "logic" $ do
    it "understands the first example" $
      checksum (followCourse1 startLoc exampleInputVals) == 150
    it "understands the second example" $
      checksum (followCourse2 startLoc exampleInputVals) == 900

run1 :: Course -> IO ()
run1 ncs =
  TextIO.putStrLn [qc|Following the course yields checksum {chk}|]
  where
    chk = checksum (followCourse1 startLoc ncs)

run2 :: Course -> IO ()
run2 ncs =
  TextIO.putStrLn [qc|Following the course yields checksum {chk}|]
  where
    chk = checksum (followCourse2 startLoc ncs)

main :: Task.Main
main = Task.mkParsedMain test (parser, run1) (parser, run2)
