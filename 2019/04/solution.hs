main = interact $
    show . solve . map read . words


-- exhaustive check from start to finish
solve :: [Int] -> Int
solve (a:b:xs) = length [x | x <- [a..b], valid x]
solve xs = -1


-- -- part 1
-- valid :: Int -> Bool
-- valid n = n<1000000 &&  increasing (xs) && hasMultiple(xs)
--     where
--         xs = unsquash(n)

-- -- part 2
valid :: Int -> Bool
valid n = n<1000000 &&  increasing (xs) && hasDouble(xs)
    where
        xs = unsquash(n)


-- has exactly 2 of some value
hasDouble :: [Int] -> Bool
hasDouble xs = any ((2 ==) . (flip countVal xs)) xs
-- hasDouble xs = any (\x -> (countVal x xs) == 2) xs

-- has at least 2 of some value
hasMultiple :: [Int] -> Bool
hasMultiple xs = any ((2 <=) . (flip countVal xs)) xs
-- hasMultiple xs = any ((\x -> countVal x xs) >= 2) xs

-- how many of a value in a list
countVal :: Eq a => a -> [a] -> Int
countVal x xs = length $ filter ((==) x) xs

-- is the list (non-strictly) increasing
increasing :: [Int] -> Bool
increasing [] = True
increasing (x:[]) = True
increasing (x1:x2:xs) = x1 <= x2 && increasing (x2:xs)




-- turn lists of digits into ints, and vice-versa
-- ============================================
addDigit :: Int -> Int -> Int
addDigit a b = 10*a + b

squash :: [Int] -> Int
squash = foldl addDigit 0


unsquash :: Int -> [Int]
unsquash n = tail (preUnsquash [n])
    where
        preUnsquash :: [Int] -> [Int]
        preUnsquash (0:xs) = 0:xs
        preUnsquash (n:xs) = preUnsquash ((div n 10):(mod n 10):xs)






-- ====================================
--          Optimisation
-- ====================================

-- -- Accumulates count in 3rd entry
-- count :: [Int] -> [Int]
-- count (c:a:b:xs) | b < a = [c]
--                  | valid a = (c+1):(next a):b:[]
--                  | otherwise = c:(next a):b:[]
-- count xs = xs

-- -- just goes to the next one
-- next :: Int -> Int
-- next = (+) 1


