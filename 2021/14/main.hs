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
solve2  (template, rules) = maximum counts - minimum counts
  where
    pairRules = Map.fromList $ map (\(k,v) -> (k, [(fst k, v), (v, snd k)])) $ Map.toList rules


    startPairCounts = getCounts $ pairs template
    finalPairCounts = (iterate step startPairCounts) !! 10

    letterCountsDoubleCounted = Map.unionWith (+) (mapKeysWith (+) fst finalPairCounts)  (mapKeysWith (+) snd finalPairCounts)
    letterCounts = (`Map.mapWithKey` letterCountsDoubleCounted) (\ k v -> 
                        case Map.lookup k endsCounts of
                          Nothing -> v `div` 2
                          Just n -> (v + n) `div` 2)

    counts = Map.elems letterCounts

    alphabet = unique $ template ++ (Map.elems rules)
    allPairs = [(a,b) | a <- alphabet, b <- alphabet]

    -- preCounts = getCounts allPairs

    stepPair pair = case Map.lookup pair pairRules of
                      Nothing -> [pair]
                      Just result -> result
    step pairCounts = Map.fromListWith (+) $ concat $ map (\(pair,count) -> map (,count) $ stepPair pair) $ Map.toList pairCounts



    s = head template
    e = last template
    -- assume `length template >= 2`
    endsCounts = if s == e 
                  then Map.fromList [(s, 2)]
                  else Map.fromList [(s,1), (e,1)]

-- not in my version of HashMap.Strict apparently
mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> Map k1 v -> Map k2 v
mapKeys f = Map.fromList . Map.foldrWithKey (\k x xs -> (f k, x) : xs) []

mapKeysWith join f = Map.fromListWith join . Map.foldrWithKey (\k x xs -> (f k, x) : xs) []

count' :: Eq a => a -> [a] -> Int
count' x xs = length $ filter (==x) xs


squash (a,b) = [a,b]
unsquash [a,b] = (a,b)



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

