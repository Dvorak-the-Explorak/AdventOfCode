main = interact $
    show . solve . read 


solve :: Int -> Int
solve x = snd $ iterateUntil (uncurry (==)) f' (f' (x,x))
          where
            f' = runners collatz
-- solve x = firstRepeat collatz x
-- solve x = takeWhile (\(x1,x2) -> x1 /= x2) (iterate (runners collatz) (collatz x, collatz $ collatz x))

takeWhileAtLeast1 :: (a -> Bool) -> [a] -> [a]
takeWhileAtLeast1 f xs = (head xs):(takeWhile f (tail xs))

-- find the first entry into the cycle
cycleEntry :: Eq a => (a->a) -> a -> (a, Int)
cycleEntry f x = iterateUntilCount (uncurry (==)) f' (x, firstMeeting f x)
                  where
                    f' = diag f
-- cycleEntry f x = length $ takeWhile different (iterate f' (x, autoCompose n f x))
--                       where 
--                         n = firstMeeting f x
--                         f' = \(x1,x2) -> (f x1, f x2)
--                         different = (\(x1,x2) -> x1 /= x2)


-- -- how long until runners collide
-- firstMeeting :: Eq a => (a->a) -> a -> a
-- firstMeeting f x = length $ takeWhileAtLeast1 different (iterate f' (x,x))
--                       where 
--                         f' = runners f
--                         different = (\(x1,x2) -> x1 /= x2)
-- firstMeeting f x = 1 + (length $ takeWhile (\(x1,x2) -> x1 /= x2) (iterate f' (f' (x,x))))
--                       where 
--                         f' = runners f

--find collision of hare and tortoise
firstMeeting :: Eq a => (a->a) -> a -> a
firstMeeting f x = iterateUntil (uncurry (==)) f' (f' (x,x))
                    where 
                      f' = runners f


iterateUntil :: (a->Bool) -> (a->a) -> a -> a 
iterateUntil pred f a | pred a = a
                      | otherwise = iterateUntil pred f (f a)

iterateUntilCount :: (a->Bool) -> (a->a) -> a -> a 
iterateUntilCount pred f x  | pred x = (x,0)
                            | otherwise = (fst fx, 1 + (snd fx))
                            where
                              fx = iterateUntil pred f (f x)

iterationsUntil :: (a->Bool) -> (a->a) -> a -> Int
iterationsUntil pred f a  | pred a = 0
                          | otherwise = 1 + (iterateUntil pred f (f a))

runners :: (a->a) -> (a,a) -> (a,a)
runners f (x1,x2) = (f x1,f (f x2))

diag :: (a->a) -> (a,a) -> (a,a)
diag f x = (f x, f x)

collatz :: Int -> Int
collatz n | n < 1 = 0
          | 2 * (div n 2) == n = div n 2
          | otherwise = 3 * n + 1


-- repeated application of f
autoCompose :: Int -> (a -> a) -> (a -> a)
autoCompose 0 f = id
autoCompose 1 f = f
autoCompose n f = f . (autoCompose (n-1) f)

diag :: a -> (a,a)
diag x = (x,x)