import Data.List (foldl')
import Data.List.Split
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import Debug.Trace

part1 = False

main = interact $
  show . solve1 . (map read) . splitOn ","

solve1 :: [Int] -> Int
solve1 xs = minimum $ map fuel [(minimum xs)..(maximum xs)]
  where
    fuel goal = sum $ map (triangle . abs . subtract goal) xs
    triangle n = if part1 
                  then n 
                  else n*(n+1) `div` 2
