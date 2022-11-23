import Data.List (foldl')
-- import Helpers (chain)


-- example
data Forest = Forest {
  _forestMap :: [[Bool]],
  _forestWidth :: Int,
  _forestHeight :: Int
}


part1 = True

main = do
  vals <- getPuzzleInput

  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  putStr "Part 2: "
  let result2 = solve2 vals
  print result2

getPuzzleInput :: IO Forest
getPuzzleInput = do
  _map <- (map (map (== '#')) . lines) <$> getContents 
  let _width = length $ head _map
  let _height = length _map
  return $ Forest _map _width _height

get :: Forest -> (Int, Int)  -> Bool
get (Forest _map _w _h) (r, c) = (_map !! r) !! (c `mod` _w)

-- ==========================================================

solve1 :: Forest-> Int
solve1 f = countTrees f (3, 1)

solve2 :: Forest -> Int
solve2 f = product $ map (countTrees f) [(1,1), (3,1), (5,1), (7,1), (1,2)]


countTrees :: Forest -> (Int, Int) -> Int
countTrees (f@(Forest _map _w _h)) (xstep, ystep) = length $ filter (get f) locations
  where
    steps = takeWhile (\n -> ystep*n < _h) [0..]
    locations = [(ystep*n, xstep*n) | n <- steps]
