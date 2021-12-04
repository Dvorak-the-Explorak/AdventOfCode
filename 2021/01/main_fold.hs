import Data.List

main = interact $
  show . solve . tripleSums . (map read) . lines


solve :: [Int] -> Int
solve [] = 0
solve xs = snd $ foldl' f (head xs, 0) (tail xs)
  where
    f (prev,count) x = (x, if x > prev then count+1 else count)

tripleSums :: [Int] -> [Int]
tripleSums = map sum . triples

triples :: [a] -> [[a]]
triples = takeWhile ((==3) . length) . tripleGen
  where
    tripleGen [] = []
    tripleGen xs = take 3 xs : tripleGen (tail xs)