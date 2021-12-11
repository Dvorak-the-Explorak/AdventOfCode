import Data.Char (digitToInt)
import Data.Maybe (catMaybes)

import Grid

import Debug.Trace
ttrace x = trace (show x) x



part1 = False

main = interact $
  show . solve . Grid . map (map digitToInt) . lines

solve :: Grid Int -> Int
solve grid = if part1 then result1 else result2
  where
    steps = iterate step grid
    result1 = sum $ map countFlashes $ take 101 steps
    result2 = length $ takeWhile (not . synced) steps
    synced = (==100) . countFlashes

countFlashes :: Grid Int -> Int
countFlashes = gridSum . (mapKernel $ simpleKernel (\x -> if x==0 then 1 else 0))

step :: Grid Int -> Grid Int
step grid = result
  where
    result = fixed (mapKernel flash) $ mapKernel increment grid

-- find fixed point
fixed :: Eq a => (a -> a) -> a -> a
fixed f x = let fx = f x in 
    if fx == x 
      then x
      else fixed f fx

increment :: Kernel Int Int
increment = simpleKernel (+1)

-- #TODO let `Kernel a b` keep track of some global state when mapping
--        (so it can track when changes happen)
flash :: Kernel Int Int
flash getVal coord = result
  where
    val = getJust $ getVal coord
    adj = catMaybes (map getVal $ allAdjacent coord)
    absorbed = length $ filter (>9) adj
    -- only flash on this call if it was already >9 (and its energy is absorbed by surrounding squids)
    -- squids on 0 already 
    result = if val == 0 || val > 9
                then 0
                else val + absorbed
