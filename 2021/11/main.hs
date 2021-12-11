import Data.Char (digitToInt)
import Data.Maybe (catMaybes)

import Grid

main = interact $
  show . solve . Grid . map (map digitToInt) . lines

solve :: Grid Int -> Int
solve x = -1

step :: Grid Int -> Grid Int
step grid = result
  where
    result = head $ filter (/= grid) $ iterate (mapKernel flash) $ mapKernel increment grid


increment :: Kernel Int Int
increment = simpleKernel (+1)

-- #TODO let `Kernel a b` keep track of some global state when mapping
flash :: Kernel Int Int
flash getVal coord = do
  val <- getVal coord
  let absorbed = length $ filter (>9) $ catMaybes (map getVal $ allAdjacent coord)
  if val == 0 || val + absorbed > 9
    then return 0
    else return val+absorbed

