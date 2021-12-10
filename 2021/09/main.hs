import Data.Char (digitToInt)
import Data.List (foldl')
import Control.Monad (liftM2)

import Debug.Trace

data Lit a = Lit (String -> String) a
instance Show a => Show (Lit a) where
  show (Lit f x) = f (show x)

prettyMB :: Maybe Bool -> Lit (Maybe Bool)
prettyMB Nothing = Lit (const "?") Nothing
prettyMB (Just True) = Lit (const "1") (Just True)
prettyMB (Just False) = Lit (const "0") (Just False)


main = interact $
  show . solve . (map (map digitToInt)) . lines

type Grid a = [[a]]
type Coord = (Int,Int)
type Kernel a b = (Coord -> Maybe a) -> Coord -> Maybe b



solve :: Grid Int -> Int
solve heights = trace (show $ map (map prettyMB) grid') (-1)
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

gridMap :: (a -> b) -> Grid a -> Grid b
gridMap = map . map

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
mapKernel kernel grid = gridMap (kernel getVal) gridCoords
  where
    -- partial function
    m = length grid
    n = length $ head grid

    getVal (row,col) 
      | row < 0 = Nothing
      | col < 0 = Nothing
      | row >= m = Nothing
      | col >= n = Nothing
      | otherwise = Just $ (grid !! row) !! col

    gridCoords :: Grid (Int,Int)
    gridCoords = [[(row,col) | col <- [0..n-1]] | row <- [0..m-1]]

traceWith f x = trace (f x) x

ttrace :: Show a => a -> a
ttrace = traceWith (show . id)