import Data.Char (digitToInt)
import Data.List (foldl')
import Control.Monad (liftM2)

import Debug.Trace

instance Show a => Show (Grid a) where
  show (Grid rows) = show rows

instance Functor Grid where
  fmap f (Grid rows) = Grid $ map (map f) rows

main = interact $
  show . solve . Grid . (map (map digitToInt)) . lines

data Grid a = Grid {
  getGrid :: [[a]]
}
type Coord = (Int,Int)
type Kernel a b = (Coord -> Maybe a) -> Coord -> Maybe b



-- partial 
getIndex :: Coord -> Grid a -> a
getIndex (row,col) (Grid x) = (x !! row) !! col

width :: Grid a -> Int
width (Grid []) = 0
width (Grid (x:xs)) = length x

height :: Grid a -> Int
height (Grid xs) = length xs



solve :: Grid Int -> Int
solve heights = trace (show grid') (-1)
-- solve heights = trace (show heights) (-1)
  where
    result = -1

    risk = (+1)

    grid' = mapKernel (isLocalMin) heights

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

isLocalMin :: Kernel Int Bool
isLocalMin getval coord = do
  center <- getval coord

  -- [Maybe Bool]
  let comparisons = map (fmap (>center) . getval) $ adjacent coord

  foldl' (<&&>) (Just True) comparisons



(<&&>) :: Maybe Bool -> Maybe Bool -> Maybe Bool
(<&&>) (Just False) _ = Just False
(<&&>) _ (Just False) = Just False
(<&&>) Nothing Nothing = Nothing
(<&&>) (Just True) _ = Just True
(<&&>) _ (Just True) = Just True


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