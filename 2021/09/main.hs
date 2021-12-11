{-# LANGUAGE BangPatterns #-}

import Data.Char (digitToInt)
import Data.List (foldl', sortOn, sort)
import Data.Maybe
import Control.Monad (liftM2, when, guard, liftM, (=<<))
import Control.Monad.Fix 
import Control.Applicative (liftA2)
import qualified  Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)

import Debug.Trace

part1 = False

main = interact $
  show . solve . Grid . (map (map digitToInt)) . lines

data Grid a = Grid {
  getGrid :: [[a]]
}
type Coord = (Int,Int)
type Kernel a b = (Coord -> Maybe a) -> Coord -> Maybe b

instance Show a => Show (Grid a) where
  show (Grid rows) = show rows

instance Functor Grid where
  fmap f (Grid rows) = Grid $ map (map f) rows

instance Applicative Grid where
  pure x = Grid [[x]]
  (<*>) (Grid f) (Grid x) = Grid $ zipWith (zipWith ($)) f x



-- partial 
getIndex :: Coord -> Grid a -> a
getIndex (row,col) (Grid x) = (x !! row) !! col

width :: Grid a -> Int
width (Grid []) = 0
width (Grid (x:xs)) = length x

height :: Grid a -> Int
height (Grid xs) = length xs

gridSum :: Grid Int -> Int
gridSum = sum . map sum . getGrid







solve :: Grid Int -> Int
solve heights = result
-- solve heights = trace (show heights) (-1)
  where
    result = if part1
              then gridSum risk
              else product $ top3

    -- [Maybe Coord]
    basinBottoms = mconcat $ getGrid $ mapKernel getBasin heights
    basinCounts = Map.filterWithKey (\ k v -> k /= Nothing) $ getCounts basinBottoms

    top3 = take 3 $ sortOn negate $ Map.elems basinCounts

    -- Grid Int
    risk = liftA2 getRisk heights localMins

    getRisk :: Int -> Maybe Bool -> Int
    getRisk n (Just True) = n+1
    getRisk _ _ = 0

    localMins = mapKernel (isLocalMin) heights


getCounts :: (Eq a, Hashable a) => [a] -> Map.HashMap a Int
getCounts = foldl' (\ acc x -> tally x acc) $ Map.fromList []
  where
    tally x = Map.insertWith (+) x 1


up :: Coord -> Coord
up (row,col) = (row-1,col)

down :: Coord -> Coord
down (row,col) = (row+1,col)

left :: Coord -> Coord
left (row,col) = (row,col-1)

right :: Coord -> Coord
right (row,col) = (row,col+1)

adjacent :: Coord -> [Coord]
adjacent x = [up x, down x, left x, right x]

simpleKernel :: (a -> b) -> Kernel a b
simpleKernel f = \getVal coord -> f <$> getVal coord
-- simpleKernel f = fmap f <$>

isLocalMin :: Kernel Int Bool
isLocalMin getval coord = do
  center <- getval coord

  -- [Maybe Bool]
  let comparisons = map (fmap (>center) . getval) $ adjacent coord

  return $ all (/= Just False) comparisons


maybeLessThan _ Nothing = True
maybeLessThan Nothing _ = False
maybeLessThan (Just x) (Just y) = x<y

-- -- not memoised
getBasin :: Kernel Int Coord
getBasin getval coord = do
  lowest <- lowestAdjacent getval coord

  if lowest == coord 
    then (Just coord)
    else getBasin getval lowest

lowestAdjacent :: Kernel Int Coord
lowestAdjacent getval coord = do
  center <- getval coord
  guard $ center == 9

  let lower c1 c2 = if maybeLessThan (getval c1) (getval c2)
                      then c1 
                      else c2
  return $ foldl' lower coord $ adjacent coord

countEqual :: Eq a => Kernel a Int
countEqual getval coord = 
    equals = filter (== (getval coord)) $ adjacent coord
  do
  center <- getval coord
  filter (==Just center) $ adjacent coord

-- run the kernel on a grid
mapKernel :: Kernel a b -> Grid a -> Grid (Maybe b)
mapKernel kernel grid = fmap (kernel getVal) gridCoords
  where

    m = height grid
    n = width grid

    getVal (row,col) 
      | row < 0 = Nothing
      | col < 0 = Nothing
      | row >= m = Nothing
      | col >= n = Nothing
      | otherwise = Just $ getIndex (row,col) grid

    gridCoords :: Grid (Int,Int)
    gridCoords = Grid [[(row,col) | col <- [0..n-1]] | row <- [0..m-1]]

traceWith f x = trace (f x) x

ttrace :: Show a => a -> a
ttrace = traceWith (show . id)