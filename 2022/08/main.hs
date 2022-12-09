{-# LANGUAGE RankNTypes #-}

import Data.List (foldl', transpose, reverse)
-- import Helpers (chain)
import Debug.Trace (trace)


ttrace x = trace (show x) x

-- example
type Grid a = [[a]]


part1 = True

main = do
  vals <- getGrid

  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  putStr "Part 2: "
  let result2 = solve2 vals
  print result2

getGrid :: IO (Grid Int)
getGrid = map (map $ read . (:[])) <$> lines <$> getContents 

showGrid :: Show a => Grid a -> String
showGrid g = "Grid:\n" ++ (foldl' (\ l r -> l ++ "\n" ++ r) "" $ map show g) ++ "\n"

countVisible :: Grid Int -> Int
countVisible grid = sum $ map (\x -> if x then 1 else 0) $ concat $ allVisible
  where
    allVisible =  foldl' fuseGrids falseGrid $ visibles
    falseGrid = map (map $ const False) grid
    visibles = map (uncurry ($)) $ zip unfuncs $ map visible grids
    grids = map ($grid) funcs
    funcs = [id, map reverse, transpose, map reverse . transpose]
    unfuncs = [id, map reverse, transpose, transpose . map reverse]
    -- directions = map ($grid) $ map visible [id, map reverse, transpose, map reverse . transpose]

-- visible :: (forall a. (Grid a -> Grid a)) -> Grid Int -> Grid Bool
visibleF f = f . (map visibleRow) . f
  where
    visibleRow = (True:) . map (\(a,b) -> a < b) . pairs

visibleRow :: [Int] -> [Bool]
visibleRow [] = []
visibleRow (x:xs) = (True:) $ go x xs
  where
    go top [] = []
    go top (x:xs) = 
      if x > top
        then (True:) $ go x xs
        else (False:) $ go top xs

visible :: Grid Int -> Grid Bool
visible = map visibleRow



fuseGrids :: Grid Bool -> Grid Bool -> [[Bool]]
fuseGrids = zipWith $ zipWith (||)

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs xs = zip xs $ tail xs


solve1 :: Grid Int -> Int
solve1 = countVisible

solve2 :: Grid Int -> Int
solve2 = const (-1)
