module Helpers where

import Data.List (foldl', sort)
import Data.Function ((&))
import Data.Hashable
import qualified Data.HashMap.Strict as Map

chain :: [a -> a] -> a -> a
chain = flip $ foldl' (&)


getCounts :: (Eq a, Hashable a) => [a] -> Map.HashMap a Int
getCounts = foldl' (\ acc x -> tally x acc) $ Map.fromList []
  where
    tally x = Map.insertWith (+) x 1

pairs :: [a] -> [(a,a)]
pairs xs = zip xs $ tail xs

unique :: (Ord a, Eq a) => [a] -> [a]
unique = trim . sort
  where
    trim (x:y:xs) | x == y = trim (x:xs)
                  | otherwise = (x:) $ trim (y:xs)
    trim xs = xs