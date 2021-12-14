{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}


import qualified Data.HashSet as Set

import BBox
import Utils
import Types
import Functions

import Debug.Trace
ttrace x = trace (show x) x



part1 = True

main = do
  (points,folds) <- getPuzzleInput

  putStr "Part 1: "
  print $ solve1 points folds

  let result = solve2 points folds
  printGrid result

solve1 :: Set Point -> [Fold] -> Int
solve1 points folds = result
  where
    result = length $ Set.toList $ doFold (head folds) points

solve2 :: Set Point -> [Fold] -> Set Point
solve2 points folds = folded
  where
    -- folded = foldl' (\ps f -> doFold f ps) points folds
    folded = doFolds folds points

