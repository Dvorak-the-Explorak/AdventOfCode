import Data.List (foldl')
-- import Helpers (chain)


-- example
type PuzzleInput = [Row]
type Row = [Int]


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
getPuzzleInput = map (map read . words) <$> lines <$> getContents 







solve1 :: PuzzleInput-> Int
solve1 = const (-1)

solve2 :: PuzzleInput -> Int
solve2 = const (-1)
