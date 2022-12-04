import Data.List (foldl')
import qualified Data.Set as Set
import Data.Char (ord)
-- import Helpers (chain)

import Debug.Trace (trace)
ttrace x = trace (show x) x

-- example
type PuzzleInput = [Row]
type Row = String


part1 = True

main = do
  vals <- getPuzzleInput

  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  putStr "Part 2: "
  let result2 = solve2 vals
  print result2

getPuzzleInput :: IO PuzzleInput
getPuzzleInput = lines <$> getContents 

solve1 :: PuzzleInput-> Int
solve1 = sum . map (value . common . halves)

solve2 :: PuzzleInput -> Int
solve2 [] = 0
-- solve2 xs = value (common $ take 3 xs) + solve2 (drop 3 xs)
solve2 (a:b:c:xs) = value (common [a,b,c]) + solve2 xs



value :: Char -> Int
value x = if ord x >= ord 'a' 
            then ord x - ord 'a' + 1
            else ord x - ord 'A' + 27

halves :: String -> [String]
halves xs = [l, r]
  where
    (l, r) = splitAt (length xs `div` 2) xs

common :: [String] -> Char
common xs = head $ Set.toList $ foldl' Set.intersection (head sets) $ tail sets
  where
    sets = map Set.fromList xs

