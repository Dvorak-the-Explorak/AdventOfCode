import Data.Function ((&))
import Data.List (foldl', scanl')

main = interact $
  (++"\n") . show . solve2


solve1 :: String -> Int
solve1  = foldl' (\ n c -> getf c n) 0
  where
    getf '(' = (+1)
    getf ')' = subtract 1


solve2 :: String -> Int
solve2  = length . takeWhile (>=0) . scanl' (\ n c -> getf c n) 0
  where
    getf '(' = (+1)
    getf ')' = subtract 1