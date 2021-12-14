import Data.List (foldl', isInfixOf)
import Helpers (pairs)

import Data.Semigroup


-- example
type PuzzleInput = [String]


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
getPuzzleInput = lines <$> getContents 


-- example
solve1 :: PuzzleInput-> Int
solve1 = length . filter valid

-- example
solve2 :: PuzzleInput -> Int
solve2 = const 0



valid :: String -> Bool
valid = getAll . (mconcat $ map (fmap All) [hasDouble, threeVowels, noProhibited])

hasDouble :: String -> Bool
hasDouble = not . null . filter (\(x,y) -> x==y) . pairs

threeVowels :: String -> Bool
threeVowels = (>=3) . length . filter (`elem` "aeiou")

noProhibited :: String -> Bool
noProhibited s = not $ any (`isInfixOf` s) ["ab", "cd", "pq", "xy"]