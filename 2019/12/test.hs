main = interact $
    show . (both sq) . heads . (map read) . words

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

sq :: Num a => a -> a
sq a = a * a

heads :: [Int] -> (Int, Int)
heads [] = (0,0)
heads (x:[]) = (0,0)
heads (x:y:xs) = (x,y)