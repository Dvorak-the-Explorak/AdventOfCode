import Data.List (foldl')
import Data.List.Split
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import Debug.Trace

part1 = False

main = interact $
  show . solve1 . (map read) . splitOn ","

ttrace x = trace (show x) x

solve1 :: [Int] -> Int
solve1 xs = localMin $ map fuel [(minimum xs)..(maximum xs)]
  where
    fuel goal = sum $ map (triangle . abs . subtract goal) xs
    triangle n = if part1 
                  then n 
                  else n*(n+1) `div` 2

-- Pretty sure 2nd deriv of `fuel goal` is increasing - works
localMin :: Ord a => [a] -> a
localMin [] = error "localMin called on empty list"
localMin [x] = x
localMin (x:y:xs) | x < y = x
                  | otherwise = localMin (y:xs) 
