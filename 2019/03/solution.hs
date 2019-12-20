import Data.HashSet (HashSet, empty, union, singleton, filter, member)

main = interact $
  show . solve . tupp . (map $ parseDirs . words) . lines

type Dir = (Char, Int)
type Dirs = [Dir]
type Point = (Int,Int)
type Path = [Point]

intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = [x | x <- xs, y<- ys, x == y]

-- 1e200 is placeholder for INF
solve :: (Dirs,Dirs) -> Int
solve (d0,d1) = consume (empty,empty) 1000000000000000 paths
  where
    paths = (tail $ followPath d0 (0,0), tail $ followPath d1 (0,0))

    consume :: (HashSet Point, HashSet Point) -> Int -> (Path,Path) -> Int
    consume (prevA,prevB) k ([],[]) = k
    consume (prevA,prevB) k ([],b:bs) | member b prevA = consume (fh prevA, fh (union prevB $ singleton b)) k' ([], fl bs)
                                      | otherwise = consume (prevA, union prevB $ singleton b) k ([], bs)
      where 
        fl list = Prelude.filter (\p -> oneNorm p < k') list
        fh hset = Data.HashSet.filter (\p -> oneNorm p < k') hset
        k' = oneNorm b
    consume (prevA,prevB) k (a:as,[]) | member a prevB = consume (fh ((union prevA $ singleton a)), fh prevB) k' (fl as, [])
                                      | otherwise = consume (union prevA $ singleton a, prevB) k (as, [])
      where 
        fl list = Prelude.filter (\p -> oneNorm p < k') list
        fh hset = Data.HashSet.filter (\p -> oneNorm p < k') hset
        k' = oneNorm a
    consume (prevA,prevB) k (a:as,b:bs) | null newInts = consume ((union prevA $ singleton a), union prevB $ singleton b) k (as, bs)
                                        | otherwise = consume (fh ((union prevA $ singleton a)), fh (union prevB $ singleton b)) k' (fl as, fl bs)
      where
        fl list = Prelude.filter (\p -> oneNorm p < k') list
        fh hset = Data.HashSet.filter (\p -> oneNorm p < k') hset
        k' = minimum $ map oneNorm newInts
        newInts = (if (member b prevA) then [b] else []) ++
                  (if (member a prevB) then [a] else []) ++
                  (if (a == b) then [a] else []) 

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

