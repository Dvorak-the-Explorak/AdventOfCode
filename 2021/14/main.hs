{-# LANGUAGE TupleSections #-}

import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import qualified Data.HashMap.Strict as Map
import Data.Hashable

import Data.List (foldl', sort, concat)
import Data.Maybe

import Helpers

import Debug.Trace
ttrace x = trace (show x) x
ttraceLabel l x = trace (l ++ ": " ++ show x) x

type Map = Map.HashMap 


-- example
type PuzzleInput = (String, Rules)
type Rules = Map (Char,Char) Char
type Rule = ((Char,Char), Char)

type PairCounts = Map (Char,Char) Int
type PairRules = Map (Char,Char) [(Char, Char)]


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

    -- counts of each pair in the template
    startPairCounts = getCounts $ pairs template
    -- counts of each pair after all the steps
    finalPairCounts = (iterate step startPairCounts) !! 40

    step = substitutePairCounts $ pairRulesFromRules rules



-- update the full string by running the rules once
substitute :: Rules -> String -> String
substitute rules (x:y:xs) = 
  case Map.lookup (x,y) rules of
    Nothing -> (x:) $ substitute rules (y:xs)
    Just result -> ([x,result] ++ ) $ substitute rules (y:xs)
substitute rules xs = xs

-- update the pair counts by running the rules once
substitutePairCounts :: PairRules -> PairCounts -> PairCounts
substitutePairCounts pairRules pairCounts = Map.fromListWith (+) $ concat $ map (\(pair,count) -> map (,count) $ stepPair pair) $ Map.toList pairCounts
  where 
    -- get the new pairs from pairRules or return the pair as a singleton list
    stepPair pair = 
      case Map.lookup pair pairRules of
        Nothing -> [pair]
        Just result -> result

-- instead of returning the inserted character, return the 2 new pairs that will exist after the update
pairRulesFromRules :: Rules -> PairRules
pairRulesFromRules rules = Map.mapWithKey (\ k v -> [(fst k, v), (v, snd k)]) rules

-- not in my version of HashMap.Strict apparently.
--    taken from the HashMap.Strict source on hackage
mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> Map k1 v -> Map k2 v
mapKeys f = Map.fromList . Map.foldrWithKey (\k x xs -> (f k, x) : xs) []

-- map keys, but add a `join :: v -> v -> v` parameter to handle key collisions in the resulting map
mapKeysWith join f = Map.fromListWith join . Map.foldrWithKey (\k x xs -> (f k, x) : xs) []


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

