{-# LANGUAGE TupleSections #-}

import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import qualified Data.HashMap.Strict as Map
import Data.Hashable

import Data.List (foldl', sort, concat)
import Data.Maybe
-- import Helpers (chain)
import Control.Monad (liftM2)

import Debug.Trace
ttrace x = trace (show x) x
ttraceLabel l x = trace (l ++ ": " ++ show x) x

type Map = Map.HashMap 


-- example
type PuzzleInput = (String, Rules)
type Rules = Map (Char,Char) Char
type Rule = ((Char,Char), Char)


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
getPuzzleInput = do
  input <- getContents
  let parseResult = parse puzzle "(unknown)" input
  case parseResult of
    (Left err) -> fail $ show err
    (Right puzzleInput) -> return puzzleInput


-- example
solve1 :: PuzzleInput-> Int
solve1 (template, rules) = most - least
  where
    subbed = (iterate (substitute rules) template) !! 10
    counts = ttrace $ Map.elems $ getCounts subbed
    most = maximum counts
    least = minimum counts

-- example
solve2 :: PuzzleInput -> Int
solve2 ([], _) = 0
solve2  (template, rules) = maximum counts - minimum counts
  where
    counts = Map.elems letterCounts

    -- counts of how many times each letter appears as the second of a pair
    secondCounts = mapKeysWith (+) snd finalPairCounts
    -- total occurences of each letter (seconds misses only the first character in the template)
    letterCounts = Map.insertWith (+) (head template) 1 secondCounts


    startPairCounts = getCounts $ pairs template
    finalPairCounts = (iterate step startPairCounts) !! 40

    -- make sure to add any characters that might appear from a rule but not be in the template
    alphabet = unique $ template ++ (Map.elems rules)
    allPairs = [(a,b) | a <- alphabet, b <- alphabet]

    stepPair pair = case Map.lookup pair pairRules of
                      Nothing -> [pair]
                      Just result -> result
    step pairCounts = Map.fromListWith (+) $ concat $ map (\(pair,count) -> map (,count) $ stepPair pair) $ Map.toList pairCounts


    pairRules = Map.mapWithKey (\ k v -> [(fst k, v), (v, snd k)]) rules

    s = head template
    e = last template
    -- `length template >= 2` because of earlier patterns in solve2 
    endsCounts = if s == e 
                  then Map.fromList [(s, 2)]
                  else Map.fromList [(s,1), (e,1)]

-- not in my version of HashMap.Strict apparently.
--    taken from the HashMap.Strict source on hackage
mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> Map k1 v -> Map k2 v
mapKeys f = Map.fromList . Map.foldrWithKey (\k x xs -> (f k, x) : xs) []

-- map keys, but add a `join :: v -> v -> v` parameter to handle key collisions in the resulting map
mapKeysWith join f = Map.fromListWith join . Map.foldrWithKey (\k x xs -> (f k, x) : xs) []


substitute :: Rules -> String -> String
substitute rules (x:y:xs) = 
  case Map.lookup (x,y) rules of
    Nothing -> (x:) $ substitute rules (y:xs)
    Just result -> ([x,result] ++ ) $ substitute rules (y:xs)
substitute rules xs = xs


getCounts :: (Eq a, Hashable a) => [a] -> Map.HashMap a Int
getCounts = foldl' (\ acc x -> tally x acc) $ Map.fromList []
  where
    tally x = Map.insertWith (+) x 1

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs xs = zip xs $ tail xs


unique :: (Ord a, Eq a) => [a] -> [a]
unique = trim . sort
  where
    trim (x:y:xs) | x == y = trim (x:xs)
                  | otherwise = (x:) $ trim (y:xs)
    trim xs = xs


-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = do
  template <- manyTill anyChar end
  end
  rules <- many1 rule
  return (template, Map.fromList rules)

--example
rule :: Parser Rule
rule = do
  l <- letter
  r <- letter
  string " -> "
  m <- letter
  end
  return ((l,r), m)




-- commonly used

integer :: Parser Int
integer = read <$> many1 digit

end :: Parser ()
end = (endOfLine >> return ()) <|> eof

