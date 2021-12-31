{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Day15
  ( main
  , test
  , spec
  ) where

import Control.Lens ((<|))
import Control.Monad (when, (>=>), zipWithM_)
import Control.Monad.ST (runST, ST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(MaybeT))
import Control.Monad.Trans.State (execStateT, modify', StateT)
import Data.Char (digitToInt)
import Data.Generics.Labels ()
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sort)
import Data.Monoid (Any(Any), getAny)
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified  Data.Vector.Generic as VG
import qualified  Data.Vector.Generic.Mutable as VGM
import qualified  Data.Vector.Unboxed as VU
import Data.Word (Word8)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.InterpolatedString.Perl6 (q, qc)
import Text.Megaparsec (parse, some, many)
import Text.Megaparsec.Char (digitChar, space)

import qualified Task
import qualified Parsers

exampleInput :: Text
exampleInput = [q|1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
|]

type Loc = Int
type Row = Int
type Col = Int
type Dims = (Col, Row)
type Point = (Col, Row)
type Risk = VU.Vector Word8
type Input = (Dims, Risk)

exampleInputVals :: Input
exampleInputVals = ((10, 10), [1,1,6,3,7,5,1,7,4,2,1,3,8,1,3,7,3,6,7,2,2,1,3,6,5,1,1,3,2,8,3,6,9,4,9,3,1,5,6,9,7,4,6,3,4,1,7,1,1,1,1,3,1,9,1,2,8,1,3,7,1,3,5,9,9,1,2,4,2,1,3,1,2,5,4,2,1,6,3,9,1,2,9,3,1,3,8,5,2,1,2,3,1,1,9,4,4,5,8,1])

-- Note, does not consume trailing space, just a single digit.
parseWord8' :: Parsers.Parser Word8
parseWord8' = fromIntegral . digitToInt <$> digitChar

parseRow :: Int -> Parsers.Parser Risk
parseRow nc = VG.replicateM nc parseWord8' <* space

parseFirstRow :: Parsers.Parser (Col, Risk)
parseFirstRow = do
  cs <- VG.fromList <$> some parseWord8'
  space
  pure (VG.length cs, cs)

parser :: Parsers.Parser (Dims, Risk)
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

neighborLocs :: Dims -> Loc -> [Loc]
neighborLocs dims@(nc, _) = fmap (toLoc nc) . neighbors dims . toPoint nc

-- point with neighbors
neighborhood :: Dims -> Point -> [Point]
neighborhood dims p = p : neighbors dims p

size :: Dims -> Int
size (nc, nr) = fromIntegral nc * fromIntegral nr

toLoc :: Col -> Point -> Loc
toLoc nc (c, r) = r * nc + c

toPoint :: Col -> Loc -> Point
toPoint nc loc = (c, r)
  where
   (r, c) = divMod loc nc

-- | Location of top left corner.
startLoc :: Loc
startLoc = 0

-- | Location of bottom right corner.
endLoc :: Dims -> Loc
endLoc dims = size dims - 1

-- | Manhattan distance between points.
manhattan :: Point -> Point -> Cost
manhattan (c1, r1) (c2, r2) = dc + dr
  where
    dc = abs (c2 - c1)
    dr = abs (r2 - r1)

atEndLoc :: Dims -> Loc -> Bool
atEndLoc dims loc = loc == size dims - 1

-- Included in transformers-0.6, but other dependencies dictate that
-- we use 0.5 at the moment.
hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

minViewWithKey' :: Applicative m => IntMap a -> MaybeT m ((IntMap.Key, a), IntMap a)
minViewWithKey' = hoistMaybe . IntMap.minViewWithKey

insert :: Ord a => IntMap.Key -> Set a -> IntMap (Set a) -> IntMap (Set a)
insert = IntMap.insertWith Set.union

insert' :: Ord a => IntMap.Key -> a -> IntMap (Set a) -> IntMap (Set a)
insert' k v = insert k [v]

elem' :: (Eq a, Foldable t1, Foldable t2) => a -> t2 (t1 a) -> Bool
elem' v =
  let fVal = Any . (== v)
      fSet = Any . getAny . foldMap fVal
  in getAny . foldMap fSet

delete' :: (Functor f, Ord a) => a -> f (Set a) -> f (Set a)
delete' v = fmap (Set.delete v)

-- Insert [v] at k in IntMap, taking care to remove any
-- instance of v at any other key in the map.
--
-- FIXME: Maybe we don't really have to remove the previous
-- instances of v and can just keep them around. If this is
-- correct, we'll trade some memory use for shorter runtime.
upsert' :: Ord a => IntMap.Key -> a -> IntMap (Set a) -> IntMap (Set a)
upsert' k v m | elem' v m = insert' k v (delete' v m)
              | otherwise = insert' k v m

type Cost = Int
findPath :: Dims -> Risk -> Maybe (Cost, Seq Loc)
findPath dims@(nc, _) risk =
  buildPathWithCost <$> search
  where
    search = runST $ do
      -- Init mutable bookkeeping vectors. We'll probably visit most
      -- locations, so a compact representation with fast random
      -- lookup makes good sense.
      pathCosts  <- VGM.replicate (size dims) maxBound
      parentLocs <- VGM.replicate (size dims) minBound
      VGM.write pathCosts startLoc 0

      -- implementation
      let
        go ::
          IntMap (Set Loc) ->
          MaybeT (ST _) (VU.Vector Cost, VU.Vector Loc)
        go = minViewWithKey' >=> handleMinOpen

        handleMinOpen ::
          ((Cost, Set Loc), IntMap (Set Loc)) ->
          MaybeT (ST _) (VU.Vector Cost, VU.Vector Loc)
        handleMinOpen ((totalCost, locs), open) =
          maybe (go open) (handleLocSet open totalCost) (Set.minView locs)

        handleLocSet ::
          IntMap (Set Loc) -> Cost -> (Loc, Set Loc) ->
          MaybeT (ST _) (VU.Vector Cost, VU.Vector Loc)
        handleLocSet open totalCost (loc, rest) = handleOpenLoc open' loc
          where
            open' | Set.null rest = open
                  | otherwise     = insert totalCost rest open

        handleOpenLoc ::
          IntMap (Set Loc) -> Loc ->
          MaybeT (ST _) (VU.Vector Cost, VU.Vector Loc)
        handleOpenLoc open loc
          | atEndLoc' loc = lift finalize
          | otherwise     = updateAllNeighbors open loc >>= go

        updateAllNeighbors ::
          IntMap (Set Loc) -> Loc -> MaybeT (ST _) (IntMap (Set Loc))
        updateAllNeighbors open loc = do
          locPathCost <- lift (VGM.read pathCosts loc)
          flip execStateT open $ do
            mapM_ (updateNeighbor loc locPathCost) (neighborLocs' loc)

        updateNeighbor ::
          Loc -> Cost -> Loc -> StateT (IntMap (Set Loc)) (MaybeT (ST _)) ()
        updateNeighbor loc locPathCost neighLoc = do
          oldNeighPathCost <- lift (VGM.read pathCosts neighLoc)
          let pathCost = locPathCost + fromIntegral (risk VU.! neighLoc)
          when (pathCost < oldNeighPathCost) $ do
            -- Found a less costly path to neighLoc (remember that the
            -- initial cost for all locations except the starting
            -- location is maxBound). Update the books!
            writeLoc neighLoc loc pathCost
            -- Add neighbor to the set of open nodes, possibly
            -- removing it if it was already present at a higher total
            -- cost.
            modify' (upsert' (manhattan' neighLoc + pathCost) neighLoc)

        writeLoc loc locParent pathCost = do
          lift $ do
            VGM.write parentLocs loc locParent
            VGM.write pathCosts  loc pathCost

        finalize :: ST _ (VU.Vector Cost, VU.Vector Loc)
        finalize =
          (,) <$> VG.unsafeFreeze pathCosts <*> VG.unsafeFreeze parentLocs

      -- Actually run search
      runMaybeT (go open0)

    endLoc' = endLoc dims

    atEndLoc' :: Loc -> Bool
    atEndLoc' = atEndLoc dims

    neighborLocs' :: Loc -> [Loc]
    neighborLocs' = neighborLocs dims

    manhattan' :: Loc -> Cost
    manhattan' = manhattan endPoint' . toPoint nc
      where
        endPoint' = toPoint nc endLoc'

    -- The initial "set" of open locations, consisting only of
    -- startLoc. The "set" is realy an IntMap keyed by total cost of
    -- each location, so we can quickly pick out the locations at the
    -- minimum cost for each iteration of the search algorithm. ... It
    -- does make it more cumbersome to update locations in the open
    -- set, though.
    open0 :: IntMap (Set Loc)
    open0 = [(totCost, [startLoc])]
      where
        -- Total cost for starting location is simply the manhattan
        -- distance. There is no path cost yet.
        totCost = manhattan' startLoc

    -- Reconstruct the sequence of locations going from startLoc to
    -- endLoc' at minimum cost, using state data yielded by 'search'.
    buildPathWithCost (pathCosts, parentLocs) =
      let go path loc
            | loc == startLoc = loc <| path
            | otherwise =
              let p = parentLocs VG.! loc
              in go (loc <| path) p
      in (pathCosts  VG.! endLoc', go mempty endLoc')

extendInput :: Input -> Input
extendInput (dims0@(nc0, nr0), risk0) = runST $ do
  let
    mul = 5 -- no. of tiles in both x and y dimensions
    nc = nc0 * mul
    nr = nr0 * mul
    dims = (nc, nr)

    -- Get the value at loc0 in risk0 and spread it across all 5*5
    -- tiles, modifying it for each tile according to their manhattan
    -- distance from the top left tile.
    multiWrite risk loc0 = zipWithM_ (VGM.write risk) allLocs allVals
      where
        -- Point corresponding to loc0 in the original risk array.
        (c0, r0) = toPoint nc0 loc0

        -- Point corresponding to loc0 in the extended risk array tile
        -- located at the given tile point.
        mkPoint (kc, kr) = (c0 + kc*nc0, r0 + kr*nr0)

        -- All 5*5 tile points. Point (0, 0) denotes the tile
        -- containing the original risk array, and (4,4) is the bottom
        -- right tile.
        tilePoints = [(kc, kr) | kr <- [0 .. mul - 1], kc <- [0 .. mul - 1]]

        -- Locations corresponding to loc0 in all tiles.
        allLocs = toLoc nc . mkPoint <$> tilePoints

        -- Value at loc0 in the top left tile.
        val0 = fromIntegral @Word8 @Int (risk0 VG.! loc0)

        -- Calculate value corresponding to val0 in the given tile.
        mkVal tilePoint =
          fromIntegral @Int @Word8
          (1 + ((val0 + manhattan (0, 0) tilePoint - 1) `mod` 9))

        -- Values corresponding to the points in allLocs.
        allVals = mkVal <$> tilePoints

  risk <- VGM.new (size dims)
  mapM_ (multiWrite risk) ([0 .. size dims0 - 1] :: [Int])
  (dims,) <$> VG.unsafeFreeze risk

test :: IO ()
test = Task.hspec spec

spec :: Spec
spec = do
  describe "parser" $ do

    it "parses example input" $
      parse parser "" exampleInput `shouldParse` exampleInputVals

  describe "logic" $ do

    it "finds full neighborhood" $ do
      sort (neighborhood (5, 5) (2, 2)) `shouldBe`
        sort
        [       (2,1)
        , (1,2),(2,2),(3,2)
        ,       (2,3)
        ]

    it "finds top left corner neighbouhood" $ do
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
        , (0,78), (1,78)
        , (0,79)
        ]

    it "finds the example path" $ do
      let (dims, risk) = exampleInputVals
      findPath dims risk `shouldBe`
        Just (40, [0,10,20,21,22,23,24,25,26,36,37,47,48,58,68,78,88,89,99])
        -- equiv. Just (40, [0,10,20,21,22,23,24,25,26,36,37,47,57,58,68,78,88,89,99])

    it "finds the extended example path cost" $ do
      let (dims, risk) = extendInput exampleInputVals
      fst <$> findPath dims risk `shouldBe`
        Just 315

run1 :: Input -> IO ()
run1 (dims, risk) =
  maybe err report (findPath dims risk)
  where
    err = TextIO.putStrLn [qc|Huh. Could not find *any* paths.|]
    report (cost, path) = do
      TextIO.putStrLn [qc|The lowest risk path has a risk of {cost}.|]
      TextIO.putStrLn [qc|It is {length path} steps long.|]

run2 :: Input -> IO ()
run2 input =
  maybe err report (findPath dims risk)
  where
    (dims, risk) = extendInput input
    err = TextIO.putStrLn [qc|Huh. Could not find *any* paths.|]
    report (cost, path) = do
      TextIO.putStrLn [qc|The lowest risk path has a risk of {cost}.|]
      TextIO.putStrLn [qc|It is {length path} steps long.|]

main :: Task.Main
main = Task.mkParsedMain test (parser, run1) (parser, run2)
