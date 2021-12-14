import Data.List (foldl', isInfixOf)
import Helpers (pairs)

import Data.Semigroup

import qualified Data.HashMap.Strict as Map
type Map = Map.HashMap

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
solve1 = length . filter valid1

-- example
solve2 :: PuzzleInput -> Int
solve2 = length . filter valid2


allP preds = getAll . (mconcat $ map (fmap All) preds)


valid1 :: String -> Bool
valid1 = allP [hasDouble, threeVowels, noProhibited]

valid2 = allP [hasPairTwice, hasNearDouble]

-- Wish I had some of that regex
hasPairTwice :: String -> Bool
hasPairTwice s = any secondPair [0..(length s)-2]
  where
    secondPair n = (pair n `isInfixOf` prefix n) || (pair n `isInfixOf` suffix n)
    pair n = take 2 $ drop n s
    prefix n = take n s
    suffix n = drop (n+2) s

hasNearDouble :: String -> Bool
hasNearDouble s = any nearDouble [0..(length s)-3]
  where
    nearDouble n = (s!!n) == (s!!(n+2)) && (s!!n) /= (s!!(n+1))


hasDouble :: String -> Bool
hasDouble = not . null . filter (\(x,y) -> x==y) . pairs

threeVowels :: String -> Bool
threeVowels = (>=3) . length . filter (`elem` "aeiou")

noProhibited :: String -> Bool
noProhibited s = not $ any (`isInfixOf` s) ["ab", "cd", "pq", "xy"]