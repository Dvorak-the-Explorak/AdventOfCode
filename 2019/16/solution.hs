import Control.Monad (liftM, ap)

main = interact $
    show . solveB . (map read) . chars


solveA :: [Int] -> Int
solveA = squash . (take 8) . (autoCompose calcPhase4 100)
-- solve :: [Int] -> [(Int,Int)]
-- solve = zip (createPattern 1)


solveB :: [Int] -> Int
solveB xs = squash $ take 8 $ drop n $ autoCompose calcPhase4 1000 $ calcPhase4 x1000
-- solveB xs = calcPhase4 $ foldr (++) [] $ replicate 10000 $ xs
  where
    x1000 = foldr (++) [] $ replicate 10000 xs
    n = squash $ take 7 $ xs
-- solveB :: [Int] -> (Int,Int,Int)
-- solveB xs = (l0,l1,l2)
--   where
--     l0 = length xs
--     l1 = length $ calcPhase $ foldr (++) [] $ replicate 10000 $ xs
--     l2 = length $ calcPhase $ calcPhase $ foldr (++) [] $ replicate 10000 $ xs
--     n = squash $ take 7 $ xs

chars :: String -> [String]
chars [] = []
chars (x:xs) = [x]:(chars xs)

createPattern :: Int -> [Int]
createPattern n = tail $ cycle $ widen n base
  where
    base = [0,1,0,-1]
    widen n xs = foldr (++) [] $ map (replicate n) xs

-- calculate one phase
step :: [Int] -> [Int]  -> Int
step xs pat = (`mod` 10) $ abs $ sum $ map (uncurry (*)) $ zip xs pat
-- step xs pat =  map (uncurry (*)) (zip xs pat)

stepn :: [Int] -> Int  -> Int
stepn xs n = step xs $ createPattern n


-- uses the base pattern 0,1,0,-1 and optimises with that info
stepn2 :: [Int] -> Int -> Int
stepn2 xs n = (`mod` 10) $ abs $ stepn2' (0:xs) n 0
  where
    stepn2' :: [Int] -> Int -> Int -> Int
    stepn2' _xs 0 _acc = _acc
    stepn2' [] _n _acc = _acc
    stepn2' xs n _acc = stepn2' (drop (n*4) xs) n $ _acc + (sum $ take n $ drop n xs) - (sum $ take n $ drop (3*n) xs)

stepn3 :: [Int] -> Int -> Int
stepn3 xs n = (`mod` 10) $ abs $ calc $ split (0:xs) n
  where
    calc :: [[Int]] -> Int
    calc chunks = calc' chunks 0
    calc' :: [[Int]] -> Int -> Int
    calc' [] acc = acc
    calc' (c1:[]) acc = acc
    calc' (c1:c2:[]) acc =  acc + sum c2
    calc' (c1:c2:c3:[]) acc = acc + sum c2
    calc' (c1:c2:c3:c4:chunks) acc = calc' chunks $! (acc + (sum c2) - (sum c4))
    split :: [Int] -> Int -> [[Int]]
    split [] n = []
    split xs n = (take n xs):(split (drop n xs) n)
    -- split xs n = [(take n xs):(take n)(drop n xs):(take n)(drop (2*n) xs):(take n)(drop (3*n) xs):]:(split (drop (4*n) xs) n)

stepnMemo :: [Int] -> [Int] -> Int -> Int
stepnMemo xs parts n  | n < (div (length xs) 2) = stepn3 xs n
                      | otherwise = (`mod` 10) $ abs $ parts !! (n-1)

-- nstep :: [Int] -> [Int]-> Int -> [Int]
-- nstep xs pat 0 = xs
-- nstep xs patn = nstep (step pat xs) (n-1)

calcPhase1 :: [Int] -> [Int]
calcPhase1 xs = map (stepn xs) [1..(length xs)]
calcPhase2 :: [Int] -> [Int]
calcPhase2 xs = map (stepn2 xs) [1..(length xs)]
calcPhase3 :: [Int] -> [Int]
calcPhase3 xs = map (stepn3 xs) [1..(length xs)]
calcPhase4 :: [Int] -> [Int]
calcPhase4 xs = map (stepnMemo xs parts) [1..(length xs)]
  where
    parts = map (\n -> sum (drop n xs)) [0..((length xs)-1)]

autoCompose :: (a->a) -> Int -> (a->a)
autoCompose f n = \xs -> iterate f xs !! n


squash :: [Int] -> Int
squash = foldl addDigit 0
  where
    addDigit :: Int -> Int -> Int
    addDigit a b = 10*a + b
