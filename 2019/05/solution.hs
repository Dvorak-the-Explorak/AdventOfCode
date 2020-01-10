import Intcode

main = interact $
  show . debug . (map read) . words

debug c = (icGetTape c' !! 255, c')
  where
    c' = icRun $ flip icBoot [1] c

solve :: [Int] -> Int
solve = head . icGetOutputs . icRun . flip icBoot [1]