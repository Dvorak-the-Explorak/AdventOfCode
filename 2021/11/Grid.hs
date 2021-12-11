module Grid where

-- #TODO can I guarantee that the kernel center is always a valid coord?
--        maybe I can get rid of the maybe      

data Grid a = Grid {
  getGrid :: [[a]]
} deriving (Eq)

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




up :: Coord -> Coord
up (row,col) = (row-1,col)

down :: Coord -> Coord
down (row,col) = (row+1,col)

left :: Coord -> Coord
left (row,col) = (row,col-1)

right :: Coord -> Coord
right (row,col) = (row,col+1)

orthoAdjacent :: Coord -> [Coord]
orthoAdjacent x = map ($x) [up, down, left, right]

allAdjacent x = map ($x) [up.left, up, up.right, left, right, down.left, down, down.right] 

simpleKernel :: (a -> b) -> Kernel a b
simpleKernel f = \getVal coord -> f <$> getVal coord
-- simpleKernel f = fmap f <$>



-- run the kernel on a grid
--  #TODO only call (kernel _ x) on valid coordinates x (type level?)
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
