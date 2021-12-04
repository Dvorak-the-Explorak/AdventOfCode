import Data.List

main = interact $
  show . solve . tripleSums . (map read) . lines

-- part 1
-- main = interact $
--   show . solve . (map read) . lines


solve :: [Int] -> Int
solve [] = 0
solve xs = snd $ foldl' tallyIncreases (head xs, 0) (tail xs)
  where
    tallyIncreases (prev,count) x = (x, if x > prev then count+1 else count)

tripleSums :: [Int] -> [Int]
tripleSums xs = zipWith3 add3 xs (tail xs) (drop 2 xs)
  where 
    add3 a b c = a + b + c



-- tripleSums :: [Int] -> [Int]
-- tripleSums = map sum . triples

-- triples :: [a] -> [[a]]
-- triples = takeWhile ((==3) . length) . tripleGen
--   where
--     tripleGen [] = []
--     tripleGen xs = take 3 xs : tripleGen (tail xs)