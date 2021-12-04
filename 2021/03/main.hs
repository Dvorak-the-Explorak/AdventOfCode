import Data.List
import Data.Char (digitToInt)
import Data.Ord (comparing)
import Debug.Trace

main = interact $
  (++"\n") . show . solvePart2 . lines 


solvePart1 :: [String] -> Int
solvePart1 input = gamma * epsilon
  where
    gamma = binToInt $ commonBits
    epsilon = 2^(length $ head input) - 1 - gamma

    commonBits = map getGammaBit $ transpose input
    getGammaBit x = if moreOnes x then '1' else '0'

solvePart2 :: [String] -> Int
solvePart2 input = oxygen * co2
  where
    oxygen = binToInt $ recurseFilter filterOxygen 0 input
    co2 = binToInt $  recurseFilter filterCo2 0 input

    filterOxygen n xs = filter (\x -> (x !! n) == (majority (map (!!n) xs))) xs
    filterCo2 n xs = filter (\x -> (x !! n) /= (majority (map (!!n) xs))) xs

    recurseFilter f n [] = error "No single item left in filtering process" 
    recurseFilter f n [x] = x
    recurseFilter f n xs = recurseFilter f (n+1) $ f n xs





ttrace x = trace (show x) x
-- ttrace x = x

binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc*2 + digitToInt x) 0

majority :: String -> Char
majority bin = if moreOnes bin then '1' else '0'

moreOnes :: String -> Bool
moreOnes bin = (>=0) $ foldl' f 0 bin
  where
    f n '1' = n+1
    f n '0' = n-1 