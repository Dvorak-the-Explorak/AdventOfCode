module Grid where    

data Grid a = Grid {
  getGrid :: [[a]]
} deriving (Eq)

-- #TODO make this 2 types: Coord (Int,Int) and ValidCoord (Int,Int)
--    then we can have getValidCoord :: ValidCoord -> thing
type Coord = (Int,Int)
-- newtype ValidCoord = ValidCood (Int,Int)

type Kernel a b = (Coord -> Maybe a) -> Coord -> b
type KernelT m a b = (Coord -> (Maybe a)) -> Coord -> m b

instance Show a => Show (Grid a) where
  show (Grid rows) = concatMap ((++"\n") . show) rows

instance Functor Grid where
  fmap f (Grid rows) = Grid $ map (map f) rows

instance Applicative Grid where
  pure x = Grid [[x]]
  (<*>) (Grid f) (Grid x) = Grid $ zipWith (zipWith ($)) f x

instance Foldable Grid where
  foldMap f (Grid vals) = foldMap (foldMap f) vals

instance Traversable Grid where
  traverse f (Grid vals) = Grid <$> traverse (traverse f) vals


-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- traverse f :: t a -> f (t b)
-- traverse (traverse f) :: (t a -> f (t b)) -> t (t a) -> f (t (t b))

-- gets numRows, numCols
gridDim :: Grid a -> (Int, Int)
gridDim (Grid []) = (0,0)
gridDim (Grid vals) = (length vals, length $ head vals)

-- partial 
getIndex :: Coord -> Grid a -> a
getIndex (row,col) (Grid x) = (x !! row) !! col

-- partial
getJust (Just x) = x

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

squareKernel x = map ($x) [up.left, up, up.right, left, id, right, down.left, down, down.right] 

fiveSquare x = [[f (g x) | f <- row] | g <- col]
  where
    row = [left.left, left, id, right, right.right] 
    col = [up.up, up, id, down, down.down]

simpleKernel :: (a -> b) -> Kernel a b
simpleKernel f = \getVal coord -> f $ getJust $ getVal coord
-- simpleKernel f = fmap f <$>



-- run the kernel on a grid
--  #TODO only call (kernel _ x) on valid coordinates x (type level?)
mapKernel :: Kernel a b -> Grid a -> Grid b
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



-- run the kernel on a grid
mapKernelT :: Applicative m => KernelT m a b -> Grid a -> m (Grid b)
mapKernelT kernel grid = traverse (kernel getVal) gridCoords
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

