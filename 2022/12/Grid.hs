module Grid where    

data Grid a = Grid {
  getGrid :: [[a]]
} deriving (Eq)

newtype Coord = Coord (Int,Int)
newtype ValidCoord = ValidCoord (Int,Int)
class HasCoord a where
  getCoord :: a -> (Int,Int)

instance HasCoord ValidCoord where
  getCoord (ValidCoord x) = x
instance HasCoord Coord where
  getCoord (Coord x) = x



type Kernel a b = HasCoord c => (c -> Maybe a) -> (ValidCoord -> a) -> ValidCoord -> b

instance Show a => Show (Grid a) where
  show (Grid rows) = show rows

instance Functor Grid where
  fmap f (Grid rows) = Grid $ map (map f) rows

instance Applicative Grid where
  pure x = Grid [[x]]
  (<*>) (Grid f) (Grid x) = Grid $ zipWith (zipWith ($)) f x



-- partial 
getIndex :: HasCoord c => c -> Grid a -> a
getIndex c (Grid x) = let (row,col) = getCoord c in (x !! row) !! col

-- partial
getJust (Just x) = x

width :: Grid a -> Int
width (Grid []) = 0
width (Grid (x:xs)) = length x

height :: Grid a -> Int
height (Grid xs) = length xs

gridSum :: Grid Int -> Int
gridSum = sum . map sum . getGrid

up :: HasCoord c => c -> Coord
up c = let (row,col) = getCoord c in (row-1,col)

down :: HasCoord c => c -> Coord
down c = let (row,col) = getCoord c in (row+1,col)

left :: HasCoord c => c -> Coord
left c = let (row,col) = getCoord c in (row,col-1)

right :: HasCoord c => c -> Coord
right c = let (row,col) = getCoord c in (row,col+1)

orthoAdjacent :: HasCoord c => c -> [Coord]
orthoAdjacent x = map ($x) [up, down, left, right]

allAdjacent x = map ($x) [up.left, up, up.right, left, right, down.left, down, down.right] 

simpleKernel :: (a -> b) -> Kernel a b
simpleKernel f = \getVal getValidVal coord -> f $ getValidVal coord



-- run the kernel on a grid
--  #TODO only call (kernel _ x) on valid coordinates x (type level?)
mapKernel :: Kernel a b -> Grid a -> Grid b
mapKernel kernel grid = fmap (kernel (getVal . getCoord) getValidVal) gridCoords
  where
    m = height grid
    n = width grid

    getVal (row,col) 
      | row < 0 = Nothing
      | col < 0 = Nothing
      | row >= m = Nothing
      | col >= n = Nothing
      | otherwise = Just $ getIndex (row,col) grid

    -- won't fail, ValidCoord is only ever made in the gridCoords function
    getValidVal (ValidCoord (row,col)) = getIndex (row,col) grid 

    gridCoords :: Grid (Int,Int)
    gridCoords = Grid [[ValidCoord (row,col) | col <- [0..n-1]] | row <- [0..m-1]]
