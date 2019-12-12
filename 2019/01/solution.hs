main = interact $
  show . solve . map read . lines


-- calculate fuel for each item, sum the totals
solve :: [Int] -> Int
solve = foldr (+) 0 . (map fuel)

-- calculate fuel for the input, without calculating fuel for that fuel
prefuel :: Int->Int
prefuel n = (-) (div n 3) 2

-- calculate fuel (handling recursive fuel requirements)
fuel :: Int -> Int
fuel n | prefuel n <= 0 = 0
       | otherwise = prefuel n + fuel (prefuel n)

