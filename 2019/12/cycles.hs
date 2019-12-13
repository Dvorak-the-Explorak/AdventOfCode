module Cycles (hareTortoise) where


-- main = interact $
--     show . solve . read 

-- solve :: Int -> Int
-- solve x = (\(x1,x2) -> x1 + x2) $ hareTortoise collatz x



hareTortoise :: Eq a => (a -> a) -> a -> (Int, Int)
hareTortoise f x = (lam, mu)
                    where 
                      lam = 1 + (snd $ iterateUntilCount (m ==) f (f m))
                      (m,mu) = cycleEntry f x



-- find the first entry into the cycle
cycleEntry :: Eq a => (a->a) -> a -> (a, Int)
cycleEntry f x = flatten $ iterateUntilCount (uncurry (==)) f' (x, m)
                  where
                    f' = diag f
                    flatten (x1,x2) = (fst x1, x2) 
                    m = firstMeeting f x

--find collision of hare and tortoise
firstMeeting :: Eq a => (a->a) -> a -> a
firstMeeting f x = fst $ iterateUntil (uncurry (==)) f' (f' (x,x))
                    where 
                      f' = runners f


iterateUntil :: (a->Bool) -> (a->a) -> a -> a 
iterateUntil pred f a | pred a = a
                      | otherwise = iterateUntil pred f (f a)

iterateUntilCount :: (a->Bool) -> (a->a) -> a -> (a,Int)
iterateUntilCount pred f x  | pred x = (x,0)
                            | otherwise = (fst fx, 1 + (snd fx))
                            where
                              fx = iterateUntilCount pred f (f x)

iterationsUntil :: (a->Bool) -> (a->a) -> a -> Int
iterationsUntil pred f a  | pred a = 0
                          | otherwise = 1 + (iterationsUntil pred f (f a))

runners :: (a->a) -> (a,a) -> (a,a)
runners f (x1,x2) = (f x1,f (f x2))

diag :: (a->a) -> (a,a) -> (a,a)
diag f (x1,x2) = (f x1, f x2)

collatz :: Int -> Int
collatz n | n < 1 = 0
          | 2 * (div n 2) == n = div n 2
          | otherwise = 3 * n + 1


-- repeated application of f
autoCompose :: Int -> (a -> a) -> (a -> a)
autoCompose 0 f = id
autoCompose 1 f = f
autoCompose n f = f . (autoCompose (n-1) f)