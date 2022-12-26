{-# LANGUAGE TupleSections #-}

import Data.List (foldl', findIndices)
-- import Helpers (chain)
import Data.Maybe (catMaybes, fromJust)
import Data.Char (ord)
import Data.Tuple.Extra (first, second)
import qualified Data.Set as Set
import Control.Monad.Reader

import Debug.Trace (trace)
ttrace x = trace (show x) x

type Set = Set.Set

-- example
type Coord = (Int, Int)
type PuzzleInput = (Coord, Coord, Grid)
type Grid = [[Int]]
type Context = Reader Grid

part1 = True

main = do
  vals <- getPuzzleInput

  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  putStr "Part 2: "
  let result2 = solve2 vals
  print result2

solve1 :: PuzzleInput -> Maybe Int
solve1 (start, end, grid) = runBfs climbUp (return . (==end)) start grid

solve2 :: PuzzleInput -> Maybe Int
solve2 (_, end, grid) = runBfs climbDown (fmap (==0) . getVal) end grid


-- Wrapper
runBfs :: (Coord -> Context [Coord]) -> -- Get adjacent coords
        (Coord -> Context Bool) ->      -- Is end coordinate
        Coord ->                        -- start
        Grid ->                         -- the grid
        Maybe Int                       -- result (possibly no path to goal)
runBfs adj isEnd start grid = runReader go grid
  where go = bfs adj isEnd Set.empty [(0,start)]


bfs :: (Coord -> Context [Coord]) ->  -- Get adjacent coords
        (Coord -> Context Bool) ->    -- Is end coordinate
        Set Coord ->                  -- visited
        [(Int, Coord)] ->             -- queue of (dist, coord to visit)
        Context (Maybe Int)           -- result (possibly no path to goal)
bfs _ _ _ [] = return Nothing
bfs adj isEnd done ((dist,curr):todo) = do
  -- Is curr the end of the search
  finished <- isEnd curr

  -- Next coords to search
  next <- filter (not . (`elem` done)) <$> adj curr

  -- Updated params after visiting current
  let done' = Set.insert curr done
  let todo' = (todo ++) $ map (dist+1,) next

  if finished then return (Just dist) else
    if (curr `elem` done) 
      then bfs adj isEnd done todo
      else bfs adj isEnd done' todo'

climbUp :: Coord -> Context [Coord]
climbUp = adjacent valid 
  where 
    valid coord next = do
      currentHeight <- getVal coord
      nextHeight <- getVal next
      return (nextHeight <= 1 + currentHeight)

climbDown :: Coord -> Context [Coord]
climbDown = adjacent valid 
  where 
    valid coord next = do
      currentHeight <- getVal coord
      nextHeight <- getVal next
      return (currentHeight <= 1 + nextHeight)

-- so slow lol
-- minimum of BFS distance from each start point
solve2_old :: PuzzleInput -> Int
solve2_old (_, end, grid) = minimum $ catMaybes $ map (\x -> solve1 (x,end,grid)) starts
  where
    startRows = findIndices  (0 `elem`) grid
    startCols = map (findIndices  (0 ==)) $ map (grid !!) startRows 
    starts :: [Coord]
    starts = concat $ zipWith (\r cs -> map (r,) cs) startRows startCols

-- ========================================================================================

-- `Reader Grid` helpers

inGrid :: Coord -> Context Bool
inGrid (r, c) = do
  rows <- reader length
  cols <- reader $ length . head
  return $ r >= 0 && r < rows && c >= 0 && c < cols

getVal :: Coord -> Context Int
getVal (row,col) = reader $ \grid -> (grid !! row) !! col

adjacent :: (Coord -> Coord -> Context Bool) -> Coord -> Context [Coord]
adjacent validStep x = plusAdjacent x >>= filterM (validStep x)

plusAdjacent :: Coord -> Context [Coord]
plusAdjacent x = do
  let coords = [first (+1) x, 
                first (subtract 1) x, 
                second (+1) x, 
                second (subtract 1) x]
  filterM inGrid coords 


-- ========================================================================================

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