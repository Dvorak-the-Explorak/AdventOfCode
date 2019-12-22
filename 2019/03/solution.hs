import Data.HashSet (HashSet, empty, union, singleton, filter, member)
import Data.Hashable
import SumOrder
import Data.List (find,findIndex)

main = interact $
  show . solveB . tupp . (map $ parseDirs . words) . lines

type Dir = (Char, Int)
type Dirs = [Dir]
type Point = (Int,Int)
type Path = [Point]


type Point2 = (Point,Int)
type Path2 = [Point2]

origin :: Point
origin = (0,0)

intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = [x | x <- xs, y<- ys, x == y]

-- 1e200 is placeholder for INF
solveA :: (Dirs,Dirs) -> Int
solveA (d0,d1) = consume (empty,empty) 1000000000000000 paths
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

addTo :: (Eq a, Hashable a) => HashSet a -> a -> HashSet a
addTo hs x = union hs $ singleton x

-- -- 1e200 is placeholder for INF
-- solveB :: (Dirs,Dirs) -> Int
-- solveB (d0,d1) = consume (empty,empty) 1000000000000000 paths
--   where
--     paths = (tail $ followPath2 d0 (origin,0), tail $ followPath2 d1 (origin,0))
--     cost (p,dist) = dist

--     consume :: (HashSet Point2, HashSet Point2) -> Int -> (Path2,Path2) -> Int
--     consume (prevA,prevB) k ([],[]) = k
--     consume (prevA,prevB) k ([],b:bs) | member b prevA = consume (prevA, addTo prevB b) k' ([], bs)
--                                       | otherwise = consume (prevA, addTo prevB b) k ([], bs)
--       where 
--         k' = cost b
--     consume (prevA,prevB) k (a:as,[]) | member a prevB = consume (addTo prevA a, prevB) k' (as, [])
--                                       | otherwise = consume (addTo prevA a, prevB) k (as, [])
--       where 
--         k' = cost a
--     consume (prevA,prevB) k (a:as,b:bs) | null newInts = consume (addTo prevA a, addTo prevB b) k (as, bs)
--                                         | otherwise = consume (addTo prevA a, addTo prevB b) k' (as, bs)
--       where
--         k' = minimum $ map cost newInts
--         newInts = (if (member b prevA) then [b] else []) ++
--                   (if (member a prevB) then [a] else []) ++
--                   (if (a == b) then [a] else []) 

solveB :: (Dirs,Dirs) -> Int
solveB (d0,d1) = case i0 of {
                    Just ii -> case i1 of{
                      Just jj -> 2 + ii + jj
                    }
                  }
  where
    i0 = findIndex (==int) p0
    i1 = findIndex (==int) p1
    int = case (find (\(a,b) -> a == b) $ sumOrder p0 p1) of {
      Just x -> fst x;
      Nothing -> (-1,-1)
    }
    p0 = tail $ followPath d0 origin
    p1 = tail $ followPath d1 origin


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


followPath2 :: Dirs -> Point2 -> Path2
followPath2 [] p = [p]
followPath2 ((d,0):dirs) p = followPath2 dirs p
followPath2 ((d,n):dirs) ((x,y),k) = ((x,y),k):(followPath2 ((d,n-1):dirs) $ move d ((x,y),k+1)) 
  where 
    move 'R' ((x,y),k) = ((x+1,y),k) 
    move 'U' ((x,y),k) = ((x,y+1),k)
    move 'L' ((x,y),k) = ((x-1,y),k)
    move 'D' ((x,y),k) = ((x,y-1),k)
