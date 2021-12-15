import Data.List (foldl')
-- import Helpers (chain)
import qualified Data.HashSet as Set
import qualified Data.PQueue.Min as PQ
import qualified Data.HashMap.Strict as Map
import Data.Char (digitToInt)

import Debug.Trace
ttrace x = trace (show x) x

type Set = Set.HashSet
type PQ = PQ.MinQueue

-- example
type PuzzleInput = Grid
type Grid = [[Int]]
type Coord = (Int,Int)
type Dists = [[Maybe Int]]

newtype Node = Node (Coord, Int)
instance Ord Node where
  compare (Node (_, risk1)) (Node (_, risk2)) = compare risk1 risk2
instance Eq Node where
  (==) (Node n1) (Node n2) = (==) n1 n2





part1 = True

main = do
  vals <- getPuzzleInput

  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  putStr "Part 2: "
  let result1 = solve2 vals
  print result1

getPuzzleInput :: IO PuzzleInput
getPuzzleInput = map (map digitToInt) <$> lines <$> getContents 


solve1 :: PuzzleInput-> Int
solve1 grid = case result of 
                Nothing -> (-1)
                Just n -> n
  where
    result = dijk grid pq visited dists bottomRight
    visited = Set.empty
    dists = setGridVal topLeft (Just 0) $ (map (map $ const Nothing)) grid
    pq = adjacentPQ grid dists topLeft 0
    (m,n) = size grid
    topLeft = (0,0)
    bottomRight = (m-1,n-1)

solve2 :: PuzzleInput -> Int
solve2 = const (-1)





-- dijk :: Grid -> PQ Node -> Set Coord -> Dists -> Coord -> Maybe Int
-- dijk grid pq visited dists end = result
--   where
--     -- result will be Nothing if pq is emtpy, otherwise apply step
--     result = step =<< PQ.minView pq

--     step (Node (coord, dist), pq') = 
--       if coord == end
--         then Just 0
--         else if coord `Set.member` visited
--           then dijk grid pq' visited dists end
--           else (dist+) <$> dijk grid (addAdjacent grid dists coord dist pq') (Set.insert coord visited) dists end



dijk :: Grid -> PQ Node -> Set Coord -> Dists -> Coord -> Maybe Int
dijk grid pq visited dists end = do
    (Node (coord, dist), pq') <- PQ.minView pq
    -- let dist' = 
    let visited' = Set.insert coord visited


    if coord == end 
      then Just dist
      else if coord `Set.member` visited
        then dijk grid pq' visited dists end
        else dijk grid (addAdjacent grid dists coord dist pq') visited' dists end





-- rows,cols
size :: Grid -> (Int,Int)
size [] = (0,0)
size grid = (length grid, length $ head grid)

addAdjacent :: Grid -> Dists ->  Coord -> Int -> PQ Node -> PQ Node
addAdjacent grid dists coord distToCoord pq = PQ.union pq $ adjacentPQ grid dists coord distToCoord

getGridVal :: [[a]] -> Coord -> a
getGridVal grid (row,col) = (grid !! row) !! col

setGridVal :: Coord -> a -> [[a]] -> [[a]]
setGridVal (m,n) x grid = result
  where
    (preRows, (theRow:postRows)) = splitAt m grid
    (preVals, (_:postVals)) = splitAt n theRow
    newRow = preVals ++ x:postVals
    result = preRows ++ newRow:postRows



adjacentPQ :: Grid -> Dists -> Coord -> Int ->  PQ Node
adjacentPQ grid dists coord distToCoord = PQ.fromList $ map makeNode $ adjacent grid coord
  where
    dist c = getGridVal dists c -- Coord -> Maybe Int
    risk c = getGridVal grid c -- Coord -> Int

    getVal c = case dist c of
              Nothing -> risk c + distToCoord -- no path there yet
              Just prevDist -> trace ("dist grid used " ++ show c ) $ min (distToCoord + risk c) prevDist
    makeNode c = Node (c, getVal c)

adjacent :: Grid -> Coord -> [Coord]
adjacent grid c@(row,col) = filter valid [up c, down c, left c, right c]
  where
    (m,n) = size grid
    valid (row,col) 
      | row < 0 = False
      | col < 0 = False
      | row >= m = False
      | col >= n = False
      | otherwise = True


up :: Coord -> Coord
up (row,col) = (row-1,col)

down :: Coord -> Coord
down (row,col) = (row+1,col)

left :: Coord -> Coord
left (row,col) = (row,col-1)

right :: Coord -> Coord
right (row,col) = (row,col+1)


