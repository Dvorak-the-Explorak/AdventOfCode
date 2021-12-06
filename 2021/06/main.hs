import Data.List (foldl')
import Data.List.Split

part1 = False

main = interact $
  show . solve . (map read) . splitOn ","

solve :: [Int] -> Int
solve input = sum $ (iterate step counts !! (if part1 then 80 else 256))
  where 
    counts = foldl' (\ acc n -> modifyAt n (+1) acc) [0,0,0,0,0,0,0,0,0] input

step :: [Int] -> [Int]
step (x:xs) = modifyAt 6 (+x) $ take 8 xs ++ [x]

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt n f xs
  | length (take (n+1) xs) < n+1 = xs
  | otherwise                 = take n xs ++ [(f $ xs !! n)] ++ drop (n+1) xs 


