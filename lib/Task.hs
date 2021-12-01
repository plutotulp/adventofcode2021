module Task
  ( Task(..)
  , toDoTask
  , DoTask(..)
  , Main
  , mkMain
  , mkParsedMain
  , hspec
  ) where

import GHC.Generics (Generic)
import System.Environment (withArgs)
import Test.Hspec (Spec)
import Test.Hspec.Core.Runner (runSpec, readConfig, defaultConfig, evaluateSummary)

import qualified Parsers

data Task
  = -- | Run the first task for the calendar day.
    Run1
    -- | Run the second task for the calendar day.
  | Run2
    -- | Run tests from development for the calendar day.
  | Test
  deriving (Show, Generic, Read)

data DoTask
  = DoRun1 FilePath
  | DoRun2 FilePath
  | DoTest

toDoTask :: Task -> Maybe FilePath -> Maybe DoTask
toDoTask = go
  where
    go Run1 (Just fp) = Just (DoRun1 fp)
    go Run2 (Just fp) = Just (DoRun2 fp)
    go Test _ = Just DoTest
    go _ _ = Nothing

type Main = DoTask -> IO ()

mkMain :: IO () -> (FilePath -> IO ()) -> (FilePath -> IO ()) -> Main
mkMain test run1 run2 = \case
  Task.DoRun1 fp -> run1 fp
  Task.DoRun2 fp -> run2 fp
  Task.DoTest -> test

mkParsedMain :: IO () -> (Parsers.Parser in1, in1 -> IO ()) -> (Parsers.Parser in2, in2 -> IO ()) -> Main
mkParsedMain test (p1, r1) (p2, r2) = \case
  Task.DoRun1 fp -> Parsers.runWithParsedInput fp p1 r1
  Task.DoRun2 fp -> Parsers.runWithParsedInput fp p2 r2
  Task.DoTest -> test

-- | Variant of 'Test.Hspec.spec' that ignores CLI arguments.
hspec :: Spec -> IO ()
hspec spec =
  readConfig defaultConfig []
  >>= withArgs [] . runSpec spec
  >>= evaluateSummary
