import Data.List (findIndex, transpose)

main = interact $
    solveB . chunksOf (25*6) . (map read) . chars


chars :: String -> [String]
chars [] = []
chars (x:xs) = [x]:(chars xs)

chunksOf :: Int -> [Int] -> [[Int]]
chunksOf n [] = []
chunksOf n xs = (take n xs):(chunksOf n (drop n xs))


solveA :: [[Int]] -> Int
solveA chunks = (count 1 best) * (count 2 best)
  where
    best = case minIndex of {
      Just i -> chunks !! i;
      Nothing -> []
    }
    -- minIndex = findIndex (== min) zeroCounts
    minIndex = elemIndex min zeroCounts
    zeroCounts = map (count 0) chunks
    min = minimum zeroCounts 

solveB :: [[Int]] -> String
solveB chunks = pretty $ map collapse $ transpose chunks
  where 
    collapse [] = 1
    collapse (2:xs) = collapse xs
    collapse (x:xs) = x



count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (== x) xs

pretty :: [Int] -> String
pretty xs = foldr (\x y -> (show x) ++ "\n" ++ y) "" rows
  where
    rows = chunksOf 25 xs