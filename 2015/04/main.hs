import System.Process
import System.Exit
import Data.List (foldl')
-- import Helpers (chain)
import Control.Monad (filterM)

import Data.Hash.MD5

import Debug.Trace
ttrace x = trace (show x) x

-- example
type PuzzleInput = String


part1 = True

main = do
  vals <- getPuzzleInput

  putStr "Part 1: "
  print $ solve1 vals

  putStr "Part 2: "
  print $ solve2 vals

getPuzzleInput :: IO PuzzleInput
getPuzzleInput = getContents 

-- example
solve1 :: PuzzleInput -> Int
solve1 input = head $ filter (valid input 5) [1..]


-- example
solve2 :: PuzzleInput -> Int
solve2 input = head $ filter (valid input 6) [1..]


valid :: String -> Int ->  Int -> Bool
valid input len n = all (=='0') $ take len $ md5s $ Str $ input ++ show n
