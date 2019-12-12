main = interact $
    show . solve . map read . words

-- exhaustive check from start to finish - using `next` to iterate (next must be strictly increasing)
solve :: [Int] -> Int
solve (a:b:xs) = length $ filter valid $ takeWhile (\x -> (x /= []) && (squash x <= b)) (iterate next (unsquash a))

-- next :: Int -> Int
-- next n | nxs == [] = 1000000
--        | otherwise = squash $ (take 6) $ nxs
--          where nxs = nextList $ unsquash n

next :: [Int] -> [Int]
next = take 6 . nextList


nextList :: [Int] -> [Int]
nextList [] = []
nextList (a:xs) | nxs /= [] = a:nxs 
                | a < 9 = (a+1):(iterate id (a+1))
                | otherwise = []
                  where nxs = nextList xs


-- change hasDouble to hasMultiple for part 1
valid :: [Int] -> Bool
valid xs = increasing (xs) && hasDouble(xs)


-- has exactly 2 of some value
hasDouble :: [Int] -> Bool
hasDouble xs = any ((2 ==) . (flip countVal xs)) xs -- flip lets you compose with the first (instead of second) argument

-- has at least 2 of some value
hasMultiple :: [Int] -> Bool
hasMultiple xs = any ((2 <=) . (flip countVal xs)) xs

-- how many of a value in a list
countVal :: Eq a => a -> [a] -> Int
countVal x xs = length $ filter ((==) x) xs

-- is the list (non-strictly) increasing.  (Checks if it's sorted)
increasing :: [Int] -> Bool
increasing [] = True
increasing (x:[]) = True
increasing (x1:x2:xs) = x1 <= x2 && increasing (x2:xs)




-- Turn Int into list of digits
-- ============================================

unsquash :: Int -> [Int]
unsquash n = tail (preUnsquash [n])
    where
        preUnsquash :: [Int] -> [Int]
        preUnsquash (0:xs) = 0:xs
        preUnsquash (n:xs) = preUnsquash ((div n 10):(mod n 10):xs)

-- un-unsquash
addDigit :: Int -> Int -> Int
addDigit a b = 10*a + b

squash :: [Int] -> Int
squash = foldl addDigit 0




-- ====================================
--          Optimisation
-- ====================================

-- -- Accumulates count in 3rd entry
-- count :: [Int] -> [Int]
-- count (c:a:b:xs) | b < a = [c]
--                  | valid a = (c+1):(next a):b:[]
--                  | otherwise = c:(next a):b:[]
-- count xs = xs


