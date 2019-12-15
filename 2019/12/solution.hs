import Cycles

main = interact $
     show . solvePart2 . (map (map read)) . (map words) . lines

-- Find total energy at time step 1000
solvePart1 :: [[Int]] -> Int
solvePart1 x = energy $ autoCompose 1000 step (x, zeroed x)

-- blegh
solvePart2 :: [[Int]] -> Int
solvePart2 x = foldr lcm 1 [solvePart2x x, solvePart2y x, solvePart2z x]
-- solvePart2 x = (uncurry (+)) $ hareTortoise step (x, zeroed x)

solvePart2x :: [[Int]] -> Int
solvePart2x x = (uncurry (+)) $ hareTortoise step (x', zeroed x')
                where
                    x' = map (\y -> [head y]) x

solvePart2y :: [[Int]] -> Int
solvePart2y x = (uncurry (+)) $ hareTortoise step (x', zeroed x')
                where
                    x' = map (\y -> [(head.tail) y]) x

solvePart2z :: [[Int]] -> Int
solvePart2z x = (uncurry (+)) $ hareTortoise step (x', zeroed x')
                where
                    x' = map (\y -> [(head.tail.tail) y]) x

-- to get initial velocity (all zero)
zeroed :: [[Int]] -> [[Int]]
zeroed = map (map (\x -> 0))

-- takes positions and velocities, updates 1 time step
step :: ([[Int]], [[Int]]) -> ([[Int]], [[Int]])
step = move . accelerate

-- update velocity
accelerate :: ([[Int]], [[Int]]) -> ([[Int]], [[Int]])
accelerate (x,v) = (x, vecSumAll v (accel x))
-- accelerate (x,v) = (x, foldr (f) v x)

-- calculates accelerations from positions
accel :: [[Int]] -> [[Int]]
accel xs = map (\x -> forces x xs) xs

-- calculate force of second argument on first
oneForce :: [Int] -> [Int] -> [Int]
oneForce x y = map (signum . (uncurry (-))) (zip y x)

-- calculate all forces on a given moon (all moons as second argument)
--      can leave x in the cs list, as oneForce x x = [0,0,0]
forces :: [Int] -> [[Int]] -> [Int]
forces x xs = foldr vecSum (head each) (tail each)
                where
                    each = (map (oneForce x) xs)
-- forces x = sum . (map (oneForce x))

-- update positions
move :: ([[Int]], [[Int]]) -> ([[Int]], [[Int]])
move (x,v) = (vecSumAll x v, v)

-- calculates total energy of the system
energy :: ([[Int]], [[Int]]) -> Int
energy x = (uncurry dotProduct) $ both (map absSum) x
-- energy x = dotProduct (map absSum (fst x)) (map absSum (snd x))

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)



-- calculates dot product of two lists
dotProduct :: [Int] -> [Int] -> Int
dotProduct x y = sum $ map (\(a,b) -> a*b) $ zip x y

vecSum :: [Int] -> [Int] -> [Int]
vecSum u v = map (uncurry (+)) (zip u v)

vecSumAll :: [[Int]] -> [[Int]] -> [[Int]]
vecSumAll us vs = map (uncurry vecSum) (zip us vs)
-- 1-norm of a list
absSum :: [Int] -> Int
absSum = sum . (map abs)

-- repeated application of f
autoCompose :: Int -> (a -> a) -> (a -> a)
autoCompose 0 f = id
autoCompose 1 f = f
autoCompose n f = f . (autoCompose (n-1) f)