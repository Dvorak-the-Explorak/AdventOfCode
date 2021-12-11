import Data.List (foldl', sort)
import qualified Data.HashMap.Strict as Map

import Debug.Trace
ttrace x = trace (show x) x

part1 = False

main = interact $ 
  show . solve . lines

solve :: [String] -> Int
solve input = if part1 then result1 else result2
  where
    scores = map (syntaxScore "") input
    result1 = sum scores
    result2 = median $ filter (>0) scores

median [] = error "median of empty list"
median xs = (sort xs) !! (length xs `div` 2)

syntaxScore :: String -> String -> Int
syntaxScore (c:cs) (x:xs)
    | x == c = syntaxScore cs xs
syntaxScore cs (x:xs) 
    | x `elem` "({[<" = syntaxScore (getClose x : cs) xs
    | otherwise       = if part1 then closeValue x else 0
syntaxScore cs [] = if part1 then 0 else finishValue cs

finishValue xs = foldl' f 0 xs
  where
    f acc ')' = 5*acc +1
    f acc ']' = 5*acc +2
    f acc '}' = 5*acc +3
    f acc '>' = 5*acc +4

closeValue ')' = 3
closeValue ']' = 57
closeValue '}' = 1197
closeValue '>' = 25137
closeValue _ = 0


closeMap :: Map.HashMap Char Char
closeMap = Map.fromList [('(', ')'),
                      ('[', ']'),
                      ('{', '}'),
                      ('<', '>')]
                      
getClose :: Char -> Char
getClose open = fromMaybe $ Map.lookup open closeMap

fromMaybe (Just x) = x