module SumOrder (sumOrder) where


main = interact $
  show . solve . read

solve :: Int -> [(Int,Int)]
solve n = sumOrder [0..5] [0..5]
-- solve n = take n $ counter $ map (\(x,y) -> x+y) $ sumOrder [0..] [0..]

counter :: Eq a => [a] -> [Int]
counter [] = []
counter (x:xs) = counter' xs x 1
  where
    counter' [] val n = [n]
    counter' (x:xs) val n | x == val = counter' xs val (n+1)
                          | otherwise = n:(counter' xs x 1)


-- -- Give elements of the cartesian product in increasing order of sum of indices
-- --   i.e order all (a[i], b[j]) by (i+j)
-- -- Works for finite or infinite lists
-- sumOrder :: [Int] -> [Int] -> [(Int,Int)]
-- sumOrder xs ys = diag [] [] (repeat empty) xs ys
--   where
--     empty :: [(Int,Int)]
--     empty = []
--     -- diag xhead   yhead    queue            xtail   ytail
--     diag :: [Int] -> [Int] -> [[(Int,Int)]] -> [Int] -> [Int] -> [(Int,Int)]
--     diag [] [] (q:qs) (x:xt) (y:yt) = (x,y):(head qs ++ (diag [x] [y] qs xt yt))
--     diag xh yh (q:qs) (x:xt) (y:yt) = (x,head yh):(head xh, y):(q ++ (diag (xh ++ [x]) (yh ++ [y]) qs' xt yt))
--       where
--         qs1' = feed qs $ map (\elem -> (elem,y)) $ tail xh
--         qs' = feed qs1' $ (map (\elem -> (x,elem)) $ (tail yh)) ++ [(x,y)]
--     diag xh yh (q:qs) [] (y:yt) = (head xh, y):(q ++ (diag xh (yh ++ [y]) qs' [] yt))
--       where
--         qs' = feed qs $ (map (\elem -> (elem,y)) $ (tail xh))
--     diag xh yh (q:qs) (x:xt) [] = (x,head yh):(q ++ (diag (xh ++ [x]) yh qs' xt []))
--       where
--         qs' = feed qs $ (map (\elem -> (x,elem)) $ (tail yh))
--     diag xh yh (q:qs) [] [] | null q = []
--                             | otherwise = q ++ (diag [][] qs [] [])
--     diag xh yh _ _ _ = [(-1,-1)]

-- Give elements of the cartesian product in increasing order of sum of indices
--   i.e order all (a[i], b[j]) by (i+j)
-- Works for finite or infinite lists
sumOrder :: [a] -> [a] -> [(a,a)]
sumOrder xs ys = diag [] [] (repeat empty) xs ys
  where
    empty :: [(a,a)]
    empty = []
    -- diag xhead   yhead    queue            xtail   ytail
    diag :: [a] -> [a] -> [[(a,a)]] -> [a] -> [a] -> [(a,a)]
    diag [] [] (q:qs) (x:xt) (y:yt) = (x,y):(head qs ++ (diag [x] [y] qs xt yt))
    diag xh yh (q:qs) (x:xt) (y:yt) = (x,head yh):(head xh, y):(q ++ (diag (xh ++ [x]) (yh ++ [y]) qs' xt yt))
      where
        qs1' = feed qs $ map (\elem -> (elem,y)) $ tail xh
        qs' = feed qs1' $ (map (\elem -> (x,elem)) $ (tail yh)) ++ [(x,y)]
    diag xh yh (q:qs) [] (y:yt) = (head xh, y):(q ++ (diag xh (yh ++ [y]) qs' [] yt))
      where
        qs' = feed qs $ (map (\elem -> (elem,y)) $ (tail xh))
    diag xh yh (q:qs) (x:xt) [] = (x,head yh):(q ++ (diag (xh ++ [x]) yh qs' xt []))
      where
        qs' = feed qs $ (map (\elem -> (x,elem)) $ (tail yh))
    diag xh yh (q:qs) [] [] | null q = []
                            | otherwise = q ++ (diag [][] qs [] [])
    diag xh yh _ _ _ = []


feed :: [[a]] -> [a] -> [[a]]
feed [] [] = []
feed lists [] = lists
feed (l1:lists) (x:xs) = (x:l1):(feed lists xs)
feed [] xs = map (\elem -> [elem]) xs