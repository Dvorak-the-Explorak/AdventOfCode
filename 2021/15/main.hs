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
    result = dijk grid pq visited bottomRight
    visited = Set.empty
    pq = adjacentPQ grid topLeft 0
    (m,n) = size grid
    topLeft = (0,0)
    bottomRight = (m-1,n-1)

solve2 :: PuzzleInput -> Int
solve2 grid = solve1 $ tileGrid grid



showGrid :: Grid -> String
showGrid grid = concatMap ((++"\n") . showRow) grid
  where showRow = concatMap show


tileGrid grid = grid'
  where
    row = rightConcat $ take 5 $ iterate incGrid grid
    grid' = downConcat $ take 5 $ iterate incGrid row

joinRight :: Grid -> Grid -> Grid
joinRight g1 g2 = zipWith (++) g1 g2

rightConcat :: [Grid] -> Grid
rightConcat [grid] = grid
rightConcat (g1:g2:grids) = rightConcat $ (joinRight g1 g2):grids

joinDown :: Grid -> Grid -> Grid
joinDown = (++)

downConcat :: [Grid] -> Grid
downConcat [grid] = grid
downConcat (g1:g2:grids) = downConcat $ (joinDown g1 g2):grids



incGrid :: Grid -> Grid
incGrid = map (map (\x -> 1 + (x `mod` 9)))


dijk :: Grid -> PQ Node -> Set Coord -> Coord -> Maybe Int
dijk grid pq visited end = do
    (Node (coord, dist), pq') <- PQ.minView pq
    -- let dist' = 
    let visited' = Set.insert coord visited


    if coord == end 
      then Just dist
      else if coord `Set.member` visited
        then dijk grid pq' visited end
        else dijk grid (addAdjacent grid coord dist pq') visited' end





-- rows,cols
size :: Grid -> (Int,Int)
size [] = (0,0)
size grid = (length grid, length $ head grid)

addAdjacent :: Grid ->  Coord -> Int -> PQ Node -> PQ Node
addAdjacent grid coord distToCoord pq = PQ.union pq $ adjacentPQ grid coord distToCoord

getGridVal :: [[a]] -> Coord -> a
getGridVal grid (row,col) = (grid !! row) !! col

setGridVal :: Coord -> a -> [[a]] -> [[a]]
setGridVal (m,n) x grid = result
  where
    (preRows, (theRow:postRows)) = splitAt m grid
    (preVals, (_:postVals)) = splitAt n theRow
    newRow = preVals ++ x:postVals
    result = preRows ++ newRow:postRows



adjacentPQ :: Grid -> Coord -> Int ->  PQ Node
adjacentPQ grid coord distToCoord = PQ.fromList $ map makeNode $ adjacent grid coord
  where
    risk c = getGridVal grid c -- Coord -> Int

    getVal c = risk c + distToCoord

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


