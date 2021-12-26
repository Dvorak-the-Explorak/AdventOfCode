import Data.List (foldl', transpose)
-- import Helpers (chain)


-- example
type Cucumbers = [[Spot]]
type Spot = Char


part1 = True

main = do
  vals <- getCucumbers

  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  putStr "Part 2: "
  let result1 = solve2 vals
  print result1

getCucumbers :: IO Cucumbers
getCucumbers = lines <$> getContents 





solve1 :: Cucumbers -> Int
solve1 state = result
  where
    state' = step state
    result = if state' == state
              then 1
              else 1 + solve1 state'

solve2 :: Cucumbers -> Int
solve2 = const (-1)


step :: Cucumbers -> Cucumbers
step state = result
  where
    state' = map moveRowRight state
    result = transpose $ map moveColDown $ transpose state'

moveRowRight :: [Spot] -> [Spot]
moveRowRight row = take n $ tail row'
  where
    row' = moveR $ [last row] ++ row ++ [head row]
    n = length row
    moveR [] = []
    moveR ('>':'.':xs) = '.':'>':(moveR xs)
    moveR (x:xs) = x:(moveR xs)


moveColDown :: [Spot] -> [Spot]
moveColDown col = take n $ tail col'
  where
    n = length col
    col' = move $ [last col] ++ col ++ [head col]
    move ('v':'.':xs) = '.':'v':(move xs)
    move (x:xs) = x:(move xs)





rot1 :: [a] -> [a]
rot1 [] = []
rot1 xs = (tail xs) ++ [head xs]
