{-# LANGUAGE TupleSections #-}

import Data.List (foldl', findIndices)
-- import Helpers (chain)
import Data.Maybe (catMaybes, fromJust)
import Data.Char (ord)
import qualified Data.Set as Set
-- import Control.Monad.Reader

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

bfs :: (Coord -> [Coord]) -> (Coord -> Bool) -> Set Coord -> [(Int, Coord)] -> Maybe Int
bfs _ _ _ [] = Nothing
bfs adj end done ((dist,curr):todo)
  | end curr = Just dist
  | curr `elem` done = bfs adj end done todo
  | otherwise = bfs adj end done' todo'
    where 
      done' = Set.insert curr done
      next = filter (not . (`elem` done)) $ adj curr
      todo' = (todo ++) $ map (dist+1,) next

climbUp :: Grid -> Coord -> [Coord]
climbUp grid coord = catMaybes $ map ($ coord) [left, right, up, down]
  where
    direction :: (Coord -> Coord) -> Coord -> Maybe Coord
    direction move coord =
      let next = move coord in
        if not (inGrid next)
          then Nothing
          else if getGridVal next <= 1 + getGridVal coord
            then Just next
            else Nothing

    inGrid :: Coord -> Bool
    inGrid (r,c) = r >= 0 && r < rows && c >= 0 && c < cols

    rows = length grid
    cols = length $ head grid

    getGridVal :: Coord -> Int
    getGridVal (row, col) = (grid !! row) !! col

    left :: Coord -> Maybe Coord
    left = direction $ \(r,c) -> (r,c-1)
    right = direction $ \(r,c) -> (r,c+1)
    up = direction $ \(r,c) -> (r-1,c)
    down = direction $ \(r,c) -> (r+1,c)


solve1 :: PuzzleInput -> Maybe Int
solve1 (start, end, grid) = bfs (climbUp grid) (==end) Set.empty $ [(0,start)]

solve2 :: PuzzleInput -> Maybe Int
solve2 (_, start, grid) = bfs paths ((==0) . getGridVal) Set.empty [(0,start)]
  where
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
solve2_old (_, end, grid) = minimum $ catMaybes $ map (\x -> solve1 (x,end,grid)) starts
  where
    startRows = findIndices  (0 `elem`) grid
    startCols = map (findIndices  (0 ==)) $ map (grid !!) startRows 
    starts :: [Coord]
    starts = concat $ zipWith (\r cs -> map (r,) cs) startRows startCols

getPuzzleInput :: IO PuzzleInput
getPuzzleInput = do
  contents <- lines <$> getContents
  let start_row = head $ findIndices ('S' `elem`) contents
  let start_col = head $ findIndices ('S' ==) $ contents !! start_row
  let end_row = head $ findIndices ('E' `elem`) contents
  let end_col = head $ findIndices ('E' ==) $ contents !! end_row
  let grid = map (map height) contents

  return ((start_row,start_col), (end_row,end_col), grid)

height :: Char -> Int
height 'S' = 0
height 'E' = 25
height c = ord c - ord 'a'