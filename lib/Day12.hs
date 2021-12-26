module Day12
  ( main
  , test
  , spec
  , Node(..)
  , (<->)
  ) where

import Control.Monad (guard)
import Data.Char (isUpper, isSpace)
import Data.Foldable (toList)
import Data.Generics.Labels ()
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified  Data.Vector as V
import qualified  Data.Vector.Generic as VG
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.InterpolatedString.Perl6 (q, qc)
import Text.Megaparsec (parse, some, takeWhile1P, single)
import Text.Megaparsec.Char (space)

import qualified Task
import qualified Parsers

exampleInput :: Text
exampleInput = [q|start-A
start-b
A-c
A-b
b-d
A-end
b-end
|]

type Name = Text

data Node
  = Big   !Name
  | Small !Name
  deriving (Eq, Ord, Show)

start,end :: Node
start = Small "start"
end   = Small "end"

hasNode :: Node -> Edge -> Bool
hasNode node (n1 :-: n2) = n1 == node || n2 == node

isSmall :: Node -> Bool
isSmall (Small _) = True
isSmall _         = False

isBig :: Node -> Bool
isBig = not . isSmall

-- | Always use '<->' constructor.
data Edge = Node :-: Node
  deriving (Eq, Show)

-- | Construct Edge with ordered nodes.
--
-- >>> Small "a" <-> Small "b" == Small "b" <-> Small "a"
-- True
(<->) :: Node -> Node -> Edge
a <-> b = n1 :-: n2
  where
    n1 = min a b
    n2 = max a b

edgeToList :: Edge -> [Node]
edgeToList (a :-: b) = [a, b]

data Graph =
  Graph
  { nodes :: Set.Set Node
  , edges :: V.Vector Edge
  }
  deriving (Eq, Show)

exampleInputVals :: Graph
exampleInputVals =
  Graph
  { nodes = Set.fromList [ start, a, b, c, d, end ]
  , edges =
      [ start <-> a
      , start <-> b
      , a     <-> c
      , a     <-> b
      , b     <-> d
      , a     <-> end
      , b     <-> end
      ]
  }
  where
    a     = Big   "A"
    b     = Small "b"
    c     = Small "c"
    d     = Small "d"

example2Input :: Text
example2Input = [q|dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
|]

example3Input :: Text
example3Input = [q|fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
|]

parseNodeName :: Parsers.Parser Text
parseNodeName = takeWhile1P (Just "node") isNameChar
  where
    isNameChar c = not (c == '-' || isSpace c)

parseNode :: Parsers.Parser Node
parseNode = mk <$> parseNodeName
  where
    mk name | isUpper (Text.head name) = Big   name
            | otherwise                = Small name

parseEdge :: Parsers.Parser Edge
parseEdge = (<->) <$> (parseNode <* single '-') <*> (parseNode <* space)

parser :: Parsers.Parser Graph
parser = do
  space
  es <- VG.fromList <$> some parseEdge
  pure $ Graph
    { nodes = Set.fromList $ concatMap edgeToList (VG.toList es)
    , edges = es
    }

vecToSeq :: VG.Vector vec a => vec a -> Seq a
vecToSeq = Seq.fromList . VG.toList

type Path = Seq Node

paths :: (V.Vector Edge -> Seq Node -> Node -> Seq Node) -> Graph -> Seq Path
paths nextNodes gr = if missingEndPoints then mempty else growPath [] [start] []
  where
    missingEndPoints =
      not (Set.member start (nodes gr) && Set.member end (nodes gr))

    nextInQueue :: Seq Path -> Seq Path -> Seq Path
    nextInQueue acc (Seq.viewl -> queue) =
      case queue of
        Seq.EmptyL -> acc
        currentPath Seq.:< laterPaths -> growPath acc currentPath laterPaths

    growPath acc currentPath@(Seq.viewr -> pps) laterPaths =
      case pps of
        Seq.EmptyR -> error "empty path really really not expected"
        _earlierNodes Seq.:> currentNode ->
          if currentNode == end then
            nextInQueue (acc Seq.:|> currentPath) laterPaths
          else
            let nextNodes' = nextNodes (edges gr) currentPath currentNode
            in case Seq.viewl ((currentPath Seq.:|>) <$> nextNodes') of
              Seq.EmptyL ->
                nextInQueue acc laterPaths
              nextPath Seq.:< moreLaterPaths ->
                growPath acc nextPath (moreLaterPaths <> laterPaths)

-- Yield all nodes reachable from current node, using rules from the
-- first task: Connected big nodes are always reachable, but connected
-- small nodes are only reachable if we've not already visited them.
nextNodes1 ::
  (VG.Vector vec Node, VG.Vector vec Edge)
  => vec Edge -> Seq Node -> Node -> Seq Node
nextNodes1 allEdges earlierNodes node = do
  vecToSeq $ flip VG.mapMaybe allEdges $ \edge -> do
    guard (hasNode node edge)
    let candidate = getOtherNode node edge
    if isBig candidate
      then Just candidate
      else maybe (Just candidate) (const Nothing)
           (Seq.elemIndexL candidate earlierNodes)

countEntries :: Eq a => a -> Seq a -> Int
countEntries node = length . Seq.elemIndicesL node

-- Yield all nodes reachable from current node, using rules from the
-- second task: We can always visit big nodes. One of the small nodes
-- can be visited twice in a single path, the rest must be visited at
-- most once.
nextNodes2 ::
  (VG.Vector vec Node, VG.Vector vec Edge)
  => vec Edge -> Seq Node -> Node -> Seq Node
nextNodes2 allEdges earlierNodes node = nextNodes
  where
    earlierSmallNodes = Seq.filter isSmall earlierNodes

    counts = flip countEntries earlierSmallNodes <$> earlierSmallNodes

    canVisitSmallNodeTwice = maximum counts < 2

    nextNodes = vecToSeq $ flip VG.mapMaybe allEdges $ \edge -> do
      guard (hasNode node edge)
      let reachableNode = getOtherNode node edge
      guard (reachableNode /= start)

      let handleSmallNode
            | reachableNode `elem` earlierNodes =
                guard canVisitSmallNodeTwice >> pure reachableNode
            | otherwise = pure reachableNode

      if isBig reachableNode then pure reachableNode else handleSmallNode

getOtherNode :: Node -> Edge -> Node
getOtherNode node (n1 :-: n2) = if n2 == node then n1 else n2

prettyPrintPath :: Path -> Text
prettyPrintPath =  Text.intercalate "," . fmap showNode . toList
  where
    showNode (Big   name) = name
    showNode (Small name) = name

test :: IO ()
test = Task.hspec spec

spec :: Spec
spec = do

  describe "parser" $ do

    it "parses first example input" $
      parse parser "" exampleInput `shouldParse` exampleInputVals

  describe "logic" $ do

    it "finds the first example paths" $ do
      Seq.sort (prettyPrintPath <$> paths nextNodes1 exampleInputVals) `shouldBe`
        Seq.sort
        [ "start,A,b,A,c,A,end"
        , "start,A,b,A,end"
        , "start,A,b,end"
        , "start,A,c,A,b,A,end"
        , "start,A,c,A,b,end"
        , "start,A,c,A,end"
        , "start,A,end"
        , "start,b,A,c,A,end"
        , "start,b,A,end"
        , "start,b,end"
        ]

    it "finds correct number of paths from the second example" $ do
      -- pattern fails if parse fails here
      let Right vals = parse parser "" example2Input
      Seq.length (paths nextNodes1 vals) `shouldBe`  19

    it "finds correct number of paths from the third example" $ do
      -- pattern fails if parse fails here
      let Right vals = parse parser "" example3Input
      Seq.length (paths nextNodes1 vals) `shouldBe`  226

    it "finds the first example paths when allowing more visits" $ do
      Seq.sort (prettyPrintPath <$> paths nextNodes2 exampleInputVals) `shouldBe`
        Seq.sort
        [ "start,A,b,A,b,A,c,A,end"
        , "start,A,b,A,b,A,end"
        , "start,A,b,A,b,end"
        , "start,A,b,A,c,A,b,A,end"
        , "start,A,b,A,c,A,b,end"
        , "start,A,b,A,c,A,c,A,end"
        , "start,A,b,A,c,A,end"
        , "start,A,b,A,end"
        , "start,A,b,d,b,A,c,A,end"
        , "start,A,b,d,b,A,end"
        , "start,A,b,d,b,end"
        , "start,A,b,end"
        , "start,A,c,A,b,A,b,A,end"
        , "start,A,c,A,b,A,b,end"
        , "start,A,c,A,b,A,c,A,end"
        , "start,A,c,A,b,A,end"
        , "start,A,c,A,b,d,b,A,end"
        , "start,A,c,A,b,d,b,end"
        , "start,A,c,A,b,end"
        , "start,A,c,A,c,A,b,A,end"
        , "start,A,c,A,c,A,b,end"
        , "start,A,c,A,c,A,end"
        , "start,A,c,A,end"
        , "start,A,end"
        , "start,b,A,b,A,c,A,end"
        , "start,b,A,b,A,end"
        , "start,b,A,b,end"
        , "start,b,A,c,A,b,A,end"
        , "start,b,A,c,A,b,end"
        , "start,b,A,c,A,c,A,end"
        , "start,b,A,c,A,end"
        , "start,b,A,end"
        , "start,b,d,b,A,c,A,end"
        , "start,b,d,b,A,end"
        , "start,b,d,b,end"
        , "start,b,end"
        ]

    it "finds correct number of paths from the second example when allowing more visits" $ do
      -- pattern fails if parse fails here
      let Right ps = paths nextNodes2 <$> parse parser "" example2Input
      Seq.length ps `shouldBe`  103

    it "finds correct number of paths from the third example when allowing more visits" $ do
      -- pattern fails if parse fails here
      let Right ps = paths nextNodes2 <$> parse parser "" example3Input
      Seq.length ps `shouldBe`  3509

run1 :: Graph -> IO ()
run1 graph =
  TextIO.putStrLn [qc|There are {n} paths through the cave system.|]
  where
    ps = paths nextNodes1 graph
    n  = Seq.length ps

run2 :: Graph -> IO ()
run2 graph =
  TextIO.putStrLn [qc|There are {n} paths through the cave system.|]
  where
    ps = paths nextNodes2 graph
    n  = Seq.length ps

main :: Task.Main
main = Task.mkParsedMain test (parser, run1) (parser, run2)
