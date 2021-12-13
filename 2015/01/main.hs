import Data.Function ((&))
import Data.List (foldl')

main = interact $
  (++"\n") . show . solve


solve :: String -> Int
solve  = foldl' (\ n c -> getf c n) 0
  where
    getf '(' = (+1)
    getf ')' = subtract 1