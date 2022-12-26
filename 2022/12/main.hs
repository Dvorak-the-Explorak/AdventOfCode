import Data.List (foldl')
-- import Helpers (chain)
import Data.Maybe (catMaybes)
import Data.Char (ord)
import qualified Data.Set as Set

import Debug.Trace (trace)
ttrace x = trace (show x) x

type Set = Set.Set

-- example
type Coord = (Int, Int)
type PuzzleInput = (Coord, Coord, Grid)
type Grid = [[Int]]

part1 = True

main = do
  vals <- getPuzzleInput

  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  putStr "Part 2: "
  let result2 = solve2 vals
  print result2

pair :: a -> b -> (a,b)
pair x y = (x,y)

solve1 :: PuzzleInput -> Int
solve1 (start, end, grid) = bfs Set.empty $ map (pair 1) $ paths start
  where
    bfs _ [] = 1000000
    bfs visited ((dist,curr):queue)
      | curr == end = dist
      | curr `elem` visited = bfs visited queue
      | otherwise = bfs (Set.insert curr visited) $ (queue ++) $ map (pair $ dist+1) $ filter (not . (`elem` visited)) $ paths curr

    rows = length grid
    cols = length $ head grid

    getGridVal :: Coord -> Int
    getGridVal (row, col) = (grid !! row) !! col

    paths :: Coord -> [Coord]
    paths x = catMaybes $ map ($ x) [left, right, up, down]

    inGrid :: Coord -> Bool
    inGrid (r,c) = r >= 0 && r < rows && c >= 0 && c < cols

    direction :: (Coord -> Coord) -> Coord -> Maybe Coord
    direction move coord =
      let next = move coord in
        if not (inGrid next)
          then Nothing
          else if getGridVal next <= 1 + getGridVal coord
            then Just next
            else Nothing

    left :: Coord -> Maybe Coord
    left = direction $ \(r,c) -> (r,c-1)
    right = direction $ \(r,c) -> (r,c+1)
    up = direction $ \(r,c) -> (r-1,c)
    down = direction $ \(r,c) -> (r+1,c)


solve2 :: PuzzleInput -> Int
solve2 (_, start, grid) = bfs Set.empty $ map (pair 1) $ paths start
  where
    bfs _ [] = 1000000
    bfs visited ((dist,curr):queue)
      | getGridVal curr == 0 = dist
      | curr `elem` visited = bfs visited queue
      | otherwise = bfs (Set.insert curr visited) $ (queue ++) $ map (pair $ dist+1) $ filter (not . (`elem` visited)) $ paths curr

    rows = length grid
    cols = length $ head grid

    getGridVal :: Coord -> Int
    getGridVal (row, col) = (grid !! row) !! col

    paths :: Coord -> [Coord]
    paths x = catMaybes $ map ($ x) [left, right, up, down]

    inGrid :: Coord -> Bool
    inGrid (r,c) = r >= 0 && r < rows && c >= 0 && c < cols

    direction :: (Coord -> Coord) -> Coord -> Maybe Coord
    direction move coord =
      let next = move coord in
        if not (inGrid next)
          then Nothing
          else if getGridVal next + 1 >= getGridVal coord
            then Just next
            else Nothing

    left :: Coord -> Maybe Coord
    left = direction $ \(r,c) -> (r,c-1)
    right = direction $ \(r,c) -> (r,c+1)
    up = direction $ \(r,c) -> (r-1,c)
    down = direction $ \(r,c) -> (r+1,c)

-- so slow lol
solve2_old :: PuzzleInput -> Int
solve2_old (_, end, grid) = minimum $ map (\x -> solve1 (x,end,grid)) starts
  where
    startRows :: [Int]
    startRows = findAll (0 `elem`) grid
    startCols :: [[Int]]
    startCols = map (findAll (0 ==)) $ map (grid !!) startRows 
    starts :: [Coord]
    starts = concat $ zipWith (\r cs -> map (pair r) cs) startRows startCols

getPuzzleInput :: IO PuzzleInput
getPuzzleInput = do
  contents <- lines <$> getContents
  let sr = find ('S' `elem`) contents
  let sc = find ('S' ==) $ contents !! sr
  let er = find ('E' `elem`) contents
  let ec = find ('E' ==) $ contents !! er
  let grid = map (map height) contents

  return ((sr,sc), (er, ec), grid)

height :: Char -> Int
height 'S' = 0
height 'E' = 25
height c = ord c - ord 'a'

find :: (a -> Bool) -> [a] -> Int
find f (x:xs) 
  | f x = 0
  | otherwise = 1 + find f xs

findAll :: (a -> Bool) -> [a] -> [Int]
findAll f [] = []
findAll f (x:xs)
  | f x = 0: map (+1) (findAll f xs)
  | otherwise = map (+1) $ findAll f xs
