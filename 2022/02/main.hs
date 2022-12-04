import Data.List (foldl')
-- import Helpers (chain)


-- example
type PuzzleInput = [String]
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
getPuzzleInput = lines <$> getContents 

solve1 :: PuzzleInput-> Int
solve1 = sum . map val1

val1 x = case x of
  "A X" -> 1 + 3
  "B Y" -> 2 + 3
  "C Z" -> 3 + 3
  "A Y" -> 2 + 6
  "B Z" -> 3 + 6
  "C X" -> 1 + 6
  "A Z" -> 3 + 0
  "B X" -> 1 + 0
  "C Y" -> 2 + 0

solve2 :: PuzzleInput -> Int
solve2 = sum . map val2

val2 x = case x of
  "A X" -> 3 + 0
  "B Y" -> 2 + 3
  "C Z" -> 1 + 6
  "A Y" -> 1 + 3
  "B Z" -> 3 + 6
  "C X" -> 2 + 0
  "A Z" -> 2 + 6
  "B X" -> 1 + 0
  "C Y" -> 3 + 3
