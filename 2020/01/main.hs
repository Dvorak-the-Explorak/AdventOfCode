import Data.List (foldl')
-- import Helpers (chain)


-- example
type PuzzleInput = [Row]
type Row = Int


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
getPuzzleInput = map read <$> lines <$> getContents 







solve1 :: PuzzleInput-> Int
solve1 vals = head $ [x*y | x <- vals, y <- vals, x + y == 2020]

solve2 :: PuzzleInput -> Int
solve2 vals = head $ [x*y*z | x <- vals, y <- vals, z <- vals, x + y + z == 2020]
