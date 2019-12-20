main = interact $
  show . solve . tupp . (map $ parseDirs . words) . lines

type Dir = (Char, Int)
type Dirs = [Dir]
type Point = (Int,Int)
type Path = [Point]

debug :: (Dirs, Dirs) -> Path
debug (d0, d1) = tail [x |  x <- followPath d0 (0,0), 
                            y <- followPath d1 (0,0),
                            x == y]

intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = [x | x <- xs, y<- ys, x == y]

-- takes O(log(forever)), but does return 1431 eventually
solveBrute :: (Dirs, Dirs) -> Int
solveBrute (path0,path1) = minimum norms
  where
    intersections = tail [x |  x <- followPath path0 (0,0), 
                          y <- followPath path1 (0,0),
                          x == y]
    norms = map oneNorm intersections
 
 --fds


-- 1e200 is placeholder for INF
solve :: (Dirs,Dirs) -> Int
solve (d0,d1) = consume ([],[]) 1000000000000000 paths
  where
    paths = (tail $ followPath d0 (0,0), tail $ followPath d1 (0,0))
    
    consume :: ([Point],[Point]) -> Int -> (Path,Path) -> Int
    consume (prevA,prevB) k ([],[]) = k
    consume (prevA,prevB) k ([],b:bs) | contains prevA b = consume (f prevA,f (b:prevB)) k' ([], f bs)
                                      | otherwise = consume (prevA, b:prevB) k ([], bs)
      where 
        f list = filter (\p -> oneNorm p < k') list
        k' = oneNorm b
    consume (prevA,prevB) k (a:as,[]) | contains prevB a = consume (f (a:prevA), f prevB) k' (f as, [])
                                      | otherwise = consume (a:prevA, prevB) k (as, [])
      where 
        f list = filter (\p -> oneNorm p < k') list
        k' = oneNorm a
    consume (prevA,prevB) k (a:as,b:bs) | null newInts = consume (a:prevA, b:prevB) k (as, bs)
                                        | otherwise = consume (f (a:prevA), f (b:prevB)) k' (f as, f bs)
      where
        f list = filter (\p -> oneNorm p < k') list
        k' = minimum $ map oneNorm newInts
        newInts = (if (contains prevA b) then [b] else []) ++
                  (if (contains prevB a) then [a] else []) ++
                  (if (a == b) then [a] else []) 



    -- consume :: ([Point],[Point],Int) -> (Path,Path) -> Int
    -- consume = consumeA . consumeB
    -- consumeA :: ([Point],[Point],Int) -> (Path,Path) -> Int
    -- consumeA (prevA,prevB,k) (as,b:bs) = consume (as,bs,k)
    -- consumeA :: ([Point],[Point],Int) -> (Path,Path) -> Int
    -- consumeA (prevA,prevB,k) (as,b:bs) = consume (as,bs,k)
    -- consumeB :: ([Point],[Point],Int) -> (Path,Path) -> Int
    -- consumeB (prevA,prevB,k) (a:as,b:bs) = consume (as,bs,k)
    -- consumeB :: ([Point],[Point],Int) -> (Path,Path) -> Int
    -- consumeB (prevA,prevB,k) (a:as,b:bs) = consume (as,bs,k)
    -- newA (prevA,prevB,k) a = 



contains :: Eq a => [a] -> a -> Bool
contains [] x = False
contains (x:xs) y | x==y = True
                  | otherwise = contains xs y

oneNorm :: (Int,Int) -> Int
oneNorm (x,y) = (abs x) + (abs y)


tupp :: [a] -> (a,a)
tupp (x:y:xs) = (x,y)

parseDirs :: [String] -> Dirs
parseDirs = map parseInstruction
  where
    parseInstruction = applyBoth (head, read . tail)

applyBoth :: (a->b,a->c) -> a -> (b,c)
applyBoth (f,g) x = (f x, g x)

followPath :: Dirs -> (Int,Int) -> Path
followPath [] (x,y) = [(x,y)]
followPath ((d,0):dirs) p = followPath dirs p
followPath ((d,n):dirs) (x,y) = (x,y):(followPath ((d,n-1):dirs) $ move d (x,y)) 
  where 
    move 'R' (x,y) = (x+1,y) 
    move 'U' (x,y) = (x,y+1)
    move 'L' (x,y) = (x-1,y)
    move 'D' (x,y) = (x,y-1)

-- follows the path to the end, but only returns elements within bound
followPathBound :: Dirs -> (Int,Int) -> Int -> Path
followPathBound ds p0 k = filter (\p -> oneNorm p <= k) $ followPath ds p0

followUntilNorm :: Dirs -> (Int,Int) -> Int -> Path
followUntilNorm dirs start k = takeWhile (\p -> oneNorm p <= k) $ followPath dirs start

-- Follow paths until bound, 
--    if no intersections within bound, extend the bound and iterate
firstIntsRelax :: (Dirs,Dirs) -> Int -> [(Int,Int)]
firstIntsRelax ds k | not $ null $ intsK = intsK
                    | otherwise = firstIntsRelax ds (k+1)
  where
    intsK = intsBounded ds k

intsBounded :: (Dirs,Dirs) -> Int -> [(Int,Int)]
intsBounded (d0,d1) k = ints (followUntilNorm d0 (0,0) k) (followUntilNorm d1 (0,0) k)
  where
    ints p0 p1 = tail [x | x <- p0, 
                      y <- p1,
                      x == y]


