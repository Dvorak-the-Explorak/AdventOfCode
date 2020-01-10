import Intcode

main = interact $
  show . solveB . (map read) . words

debug c = ((icGetTape c') !! 294, c')
  where
    c' = icRun $ flip icBoot [0] c

solveA :: [Int] -> Int
solveA = head . icGetOutputs . icRun . flip icBoot [1]


solveB :: [Int] -> Int
solveB = head . icGetOutputs . icRun . flip icBoot [5]