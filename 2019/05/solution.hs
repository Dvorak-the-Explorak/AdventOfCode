import Intcode

main = interact $
  show . solveB . (map read) . words

debug c = (c')
  where
    c' = icRun $ flip icBoot [5] c

solveA :: [Int] -> Int
solveA = last . icGetOutputs . icRun . flip icBoot [1]


solveB :: [Int] -> Int
solveB = last . icGetOutputs . icRun . flip icBoot [5]