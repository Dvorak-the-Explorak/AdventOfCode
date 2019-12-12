import Control.Arrow

-- main = do
--     n <- getLine
--     putStrLn show fib read n



-- main = interact $
--   lines >>> map (words >>> map read >>> solve >>> show) >>> unlines
-- solve :: [Integer] -> Integer
-- solve [a,b] = abs (a - b)





-- main = interact $
--   unlines . map (show . fib . read) . lines

-- main = interact $
--     show . (foldr (+) 0 (map (fib . read) . lines))


main = interact $
  show . solve . map read . lines



solve :: [Int] -> Int
solve = foldr (+) 0 . (map fib)


fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n-2)

