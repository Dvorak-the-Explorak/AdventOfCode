import Data.List (foldl', transpose, reverse)
-- import Helpers (chain)
import Debug.Trace (trace)

-- This is not a nice solution




ttrace x = trace (show x) x

type Grid a = [[a]]
data SRow a = SRow [a] a [a]
data SGrid a = SGrid [SRow a] (SRow a) [SRow a]

instance Show a => Show (SRow a) where
  show (SRow ls x rs) = unwords (map show ls) ++ "|" ++ show x ++ "|" ++ unlines (map show rs)
instance Show a => Show (SGrid a) where
  show (SGrid us x@(SRow ls _ rs) ds) = unlines (map show us) ++ 
                                        "\n" ++ (take n $ repeat '-') ++ "\n" ++ 
                                        show x ++ 
                                        "\n" ++ (take n $ repeat '-') ++ "\n" ++ 
                                        unlines (map show ds)
    where n = length ls + length rs + 1

part1 = True

main = do
  vals <- getGrid

  putStr "Part 1: "
  let result1 = solve1 vals -- $ trace (showGrid vals) vals
  print result1

  putStr "Part 2: "
  let result2 = solve2 vals
  print result2

getGrid :: IO (Grid Int)
getGrid = map (map $ read . (:[])) <$> lines <$> getContents 

showGrid :: Show a => Grid a -> String
showGrid g = "Grid:\n" ++ (unlines $ map show g) ++ "\n"

countVisible :: Grid Int -> Int
countVisible grid = countElem True $ concat $ allVisible
  where
    allVisible =  foldl' fuseGrids falseGrid $ visibles
    -- a grid of all Falses
    falseGrid = map (map $ const False) grid
    -- visible trees for each orientation
    visibles = map (flip visibleF grid) $ zip funcs unfuncs
    -- Forward and reverse orientations
    funcs = [id, map reverse, transpose, map reverse . transpose]
    unfuncs = [id, map reverse, transpose, transpose . map reverse]

visibleF :: (Grid Int -> Grid Int, Grid Bool -> Grid Bool) -> Grid Int -> Grid Bool
visibleF (f, f') = f' . map visibleRow . f

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

countElem :: Eq a => a -> [a] -> Int
countElem target = sum . map coerce . map (==target)
  where
    coerce :: Bool -> Int
    coerce True = 1
    coerce False = 0

fuseGrids :: Grid Bool -> Grid Bool -> [[Bool]]
fuseGrids = zipWith $ zipWith (||)

scenicScore :: SGrid Int -> Int
scenicScore sgrid = product $ map viewDistance $ map ($sgrid) [up, down, left, right]
  where
    viewDistance (x:xs) = go x xs
    go val [] = 0
    go val (x:xs) | val <= x = 1
                  | val > x = 1 + go val xs

center :: SRow a -> a
center (SRow _ x _) = x

up :: SGrid a -> [a]
up (SGrid rows r _) = center r : map center rows

down :: SGrid a -> [a]
down (SGrid _ r rows) = center r : map center rows

left :: SGrid a -> [a]
left (SGrid _ (SRow l x _) _) = x:l

right :: SGrid a -> [a]
right (SGrid _ (SRow _ x r) _) = x:r

split :: Grid a -> SGrid a
split ((x:xs):rs) = SGrid [] (SRow [] x xs) $ map newSRow rs

newSRow :: [a] -> SRow a
newSRow (x:xs) = SRow [] x xs

resetSRow :: SRow a -> SRow a
resetSRow (SRow ls x rs) = newSRow $ (reverse ls) ++ (x:rs)

nextSRow :: SRow a -> SRow a
nextSRow (SRow ls x (r:rs)) = SRow (x:ls) r rs

nextSGrid :: SGrid a -> SGrid a
nextSGrid sg@(SGrid _ (SRow _ _ []) []) = sg
nextSGrid (SGrid us r@(SRow ls x []) (d:ds)) = SGrid (map resetSRow $ r:us) (resetSRow d) $ map resetSRow ds
nextSGrid (SGrid us r ds) = SGrid (map nextSRow us) (nextSRow r) (map nextSRow ds)

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs xs = zip xs $ tail xs

solve1 :: Grid Int -> Int
solve1 = countVisible

solve2 :: Grid Int -> Int
solve2 g = maximum $ take n $ map scenicScore $ iterate nextSGrid (split g)
  where
    n = length g * length (head g)
