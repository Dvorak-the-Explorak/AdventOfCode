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
 
getCountsList :: [Int] -> [Int]
getCountsList xs = map (\x -> findWithDefault 0 x countsMap) [0..top]
  where
    top = maximum xs
    countsMap = getCounts xs

pairs :: [a] -> [(a,a)]
pairs xs = zip xs $ tail xs

unique :: (Ord a, Eq a) => [a] -> [a]
unique = trim . sort
  where
    trim (x:y:xs) | x == y = trim (x:xs)
                  | otherwise = (x:) $ trim (y:xs)
    trim xs = xs


findWithDefault :: (Eq k, Hashable k)
              => v          -- ^ Default value to return.
              -> k -> Map.HashMap k v -> v
findWithDefault def k t = case Map.lookup k t of
    Just v -> v
    _      -> def
{-# INLINABLE findWithDefault #-}