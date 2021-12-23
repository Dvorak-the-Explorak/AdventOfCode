import Data.List (foldl')
import qualified Data.HashSet as Set
import qualified Data.PQueue.Min as PQ
-- import Helpers (chain)
import Data.Maybe
import Control.Monad
import Data.Hashable


import Debug.Trace
ttrace x = trace (show x) x




type Set = Set.HashSet
type PQ = PQ.MinQueue



-- example
data Pod = A | B | C | D
  deriving (Eq, Show)
instance Hashable Pod where
  hashWithSalt salt A = hashWithSalt salt "A"
  hashWithSalt salt B = hashWithSalt salt "B"
  hashWithSalt salt C = hashWithSalt salt "C"
  hashWithSalt salt D = hashWithSalt salt "D"


stepCost :: Pod -> Int
stepCost A = 1
stepCost B = 10
stepCost C = 100
stepCost D = 1000

data Burrow = Burrow {
  left1 ::  Maybe Pod,
  left2 :: Maybe Pod,
  roomA :: [Pod],
  gap1 :: Maybe Pod,
  roomB :: [Pod],
  gap2 :: Maybe Pod,
  roomC :: [Pod],
  gap3 :: Maybe Pod,
  roomD :: [Pod],
  right1 :: Maybe Pod,
  right2 :: Maybe Pod
}
  deriving Eq 
instance Hashable Burrow where
  hashWithSalt salt (Burrow  l1 l2 a g1 b g2 c g3 d r1 r2) = hashWithSalt salt $ [l1, l2, g1, g2, g3, r1, r2] ++ map Just (a ++ b ++ c ++ d)
instance Show Burrow where
  show (Burrow  l1 l2 a g1 b g2 c g3 d r1 r2) = 
      "#############\n"
    ++"#" ++ showSpot l1 ++ showSpot l2 ++ "." ++ showSpot g1 ++ "." ++ showSpot g2 ++ "." ++ showSpot g3 ++ "." ++ showSpot r1 ++ showSpot r2 ++ "#\n"
    ++"###" ++ secondPod a ++ "#" ++ secondPod b ++ "#" ++ secondPod c ++ "#" ++ secondPod d ++ "###\n"
    ++"  #" ++ firstPod a ++ "#" ++ firstPod b ++ "#" ++ firstPod c ++ "#" ++ firstPod d ++ "#  \n"
    ++"  #########"

showSpot :: Maybe Pod -> String
showSpot Nothing = "."
showSpot (Just x) = show x

firstPod [] = "."
firstPod xs = show $ last xs

secondPod (x:_:_) = show x
secondPod _ = "."


data Node = Node Burrow Int
instance Ord Node where
  compare (Node _ risk1) (Node _ risk2) = compare risk1 risk2
instance Eq Node where
  (==) (Node b1 c1) (Node b2 c2) = (b1 == b2) && (c1 == c2)
instance Hashable Node where
  hashWithSalt salt (Node b c) = hashWithSalt salt (b,c)
instance Show Node where
  show (Node burrow cost) = "\n" ++ show burrow ++ " " ++ show cost ++ "\n\n"

type Move = Burrow -> Maybe Node



newtype WithPath a = WithPath (a, [a])
instance Ord a => Ord (WithPath a) where
  compare (WithPath (n1, _)) (WithPath (n2, _)) = compare n1 n2
instance Eq a => Eq (WithPath a) where
  (==) (WithPath (n1, _)) (WithPath (n2, _)) = (==) n1 n2
instance Hashable a => Hashable (WithPath a) where
  hashWithSalt salt (WithPath (n, path))= hashWithSalt salt (n,path)
instance Show a => Show (WithPath a) where
  show (WithPath (n,_)) = show n



part1 = True

main = do
  let test1 = Burrow 
                { left1 = Nothing
                , left2 = Nothing
                , roomA =[B, A]
                , gap1 =Nothing
                , roomB =[C, D]
                , gap2 =Nothing
                , roomC =[B, C]
                , gap3 =Nothing
                , roomD =[D, A]
                , right1 = Nothing
                , right2 = Nothing
                }

  let input1 = Burrow 
                { left1 = Nothing
                , left2 = Nothing
                , roomA =[A, D]
                , gap1 =Nothing
                , roomB =[C, A]
                , gap2 =Nothing
                , roomC =[B, D]
                , gap3 =Nothing
                , roomD =[C, B]
                , right1 = Nothing
                , right2 = Nothing
                }

  let test2 = Burrow 
                { left1 = Nothing
                , left2 = Nothing
                , roomA =[B, D, D, A]
                , gap1 =Nothing
                , roomB =[C, C, B, D]
                , gap2 =Nothing
                , roomC =[B, B, A, C]
                , gap3 =Nothing
                , roomD =[D, A, C, A]
                , right1 = Nothing
                , right2 = Nothing
                }
  let input2 = Burrow 
                { left1 = Nothing
                , left2 = Nothing
                , roomA =[A, D, D, D]
                , gap1 =Nothing
                , roomB =[C, C, B, A]
                , gap2 =Nothing
                , roomC =[B, B, A, D]
                , gap3 =Nothing
                , roomD =[C, A, C, B]
                , right1 = Nothing
                , right2 = Nothing
                }

  -- putStr "Part 1: "
  -- let result1 = solve1 input
  -- case result1 of
  --   Nothing -> putStrLn "Couldn't find a solution"
  --   Just (dist, path) -> do
  --     mapM print $ reverse path
  --     print dist

  putStr "Part 2: "
  let result2 = solve2 test2
  case result2 of
    Nothing -> putStrLn "Couldn't find a solution"
    Just (dist, path) -> do
      mapM print $ reverse path
      print dist



solve1 :: Burrow -> Maybe (Int, [Node])
solve1 burrow = result
  where
    result = dijkWithPath optionsWithPath pq visited end

    visited = Set.empty
    pq = optionsWithPath $ WithPath (start, [start])
    start = (Node burrow 0)
    end = Burrow 
            { left1 = Nothing
            , left2 = Nothing
            , roomA =[A, A]
            , gap1 =Nothing
            , roomB =[B, B]
            , gap2 =Nothing
            , roomC =[C, C]
            , gap3 =Nothing
            , roomD =[D, D]
            , right1 = Nothing
            , right2 = Nothing
            }



solve2 :: Burrow -> Maybe (Int, [Node])
solve2 burrow = result
  where
    result = dijkWithPath optionsWithPath pq visited end

    visited = Set.empty
    pq = optionsWithPath $ WithPath (start, [start])
    start = (Node burrow 0)
    end = Burrow 
            { left1 = Nothing
            , left2 = Nothing
            , roomA =[A, A, A, A]
            , gap1 =Nothing
            , roomB =[B, B, B, B]
            , gap2 =Nothing
            , roomC =[C, C, C, C]
            , gap3 =Nothing
            , roomD =[D, D, D, D]
            , right1 = Nothing
            , right2 = Nothing
            }








options :: Node -> PQ Node
options (Node curr dist) = result
  where
    result = PQ.fromList $ map (addCost dist) adj

    -- [Node]
    adj = catMaybes $ map ($curr) moves
    moves = [left1ToA, left1ToG1 >>> g1ToB, left1ToG1 >>> g1ToG2 >>> g2ToC
                ,left1ToG1 >>> g1ToG2 >>> g2ToG3 >>> g3ToD
            , left2ToA, left2ToG1 >>> g1ToB, left2ToG1 >>> g1ToG2 >>> g2ToC
                ,left2ToG1 >>> g1ToG2 >>> g2ToG3 >>> g3ToD
            , aToLeft1, aToLeft2, aToG1, aToG1 >>> g1ToG2, aToG1 >>> g1ToG2 >>> g2ToG3
                , aToG1 >>> g1ToG2 >>> g2ToG3 >> g3ToRight1, aToG1 >>> g1ToG2 >>> g2ToG3 >> g3ToRight2
            , g1ToA, g1ToB, g1ToG2 >>> g2ToC
                , g1ToG2 >>> g2ToG3 >>> g3ToD
            , bToG1 >>> g1ToLeft1, bToG1 >>> g1ToLeft2, bToG1, bToG2, bToG2 >>> g2ToG3
                , bToG2 >>> g2ToG3 >>> g3ToRight1, bToG2 >>> g2ToG3 >>> g3ToRight2
            , g2ToG1 >>> g1ToA, g2ToB, g2ToC
                , g2ToG3 >>> g3ToD
            , cToG2 >>> g2ToG1 >>> g1ToLeft1, cToG2 >>> g2ToG1 >>> g1ToLeft2, cToG2 >>> g2ToG1
                , cToG2, cToG3, cToG3>>> g3ToRight1, cToG3>>> g3ToRight2
            , g3ToG2 >>> g2ToG1 >>> g1ToA, g3ToG2 >>> g2ToB, g3ToC
                , g3ToD
            , dToG3 >>> g3ToG2 >>> g2ToG1 >>> g1ToLeft1, dToG3 >>> g3ToG2 >>> g2ToG1 >>> g1ToLeft2, dToG3 >>> g3ToG2 >>> g2ToG1
                , dToG3 >>> g3ToG2, dToG3, dToRight1, dToRight2
            , right1ToG3 >>> g3ToG2 >>> g2ToG1 >>> g1ToA, right1ToG3 >>> g3ToG2 >>> g2ToB, right1ToG3 >>> g3ToC
                , right1ToD
            , right2ToG3 >>> g3ToG2 >>> g2ToG1 >>> g1ToA, right2ToG3 >>> g3ToG2 >>> g2ToB, right2ToG3 >>> g3ToC
                , right2ToD]

    addCost c (Node x d) = Node x (d+c)



optionsWithPath :: WithPath Node -> PQ (WithPath Node)
optionsWithPath (WithPath ((Node curr dist), path)) = result
  where
    result = PQ.fromList $ map (addPath . addCost dist) adj

    -- [Node]12
    adj = catMaybes $ map ($curr) moves
    moves = [left1ToA, left1ToG1 >>> g1ToB, left1ToG1 >>> g1ToG2 >>> g2ToC
                ,left1ToG1 >>> g1ToG2 >>> g2ToG3 >>> g3ToD
            , left2ToA, left2ToG1 >>> g1ToB, left2ToG1 >>> g1ToG2 >>> g2ToC
                ,left2ToG1 >>> g1ToG2 >>> g2ToG3 >>> g3ToD
            , aToLeft1, aToLeft2, aToG1, aToG1 >>> g1ToG2, aToG1 >>> g1ToG2 >>> g2ToG3
                , aToG1 >>> g1ToG2 >>> g2ToG3 >> g3ToRight1, aToG1 >>> g1ToG2 >>> g2ToG3 >> g3ToRight2
            , g1ToA, g1ToB, g1ToG2 >>> g2ToC
                , g1ToG2 >>> g2ToG3 >>> g3ToD
            , bToG1 >>> g1ToLeft1, bToG1 >>> g1ToLeft2, bToG1, bToG2, bToG2 >>> g2ToG3
                , bToG2 >>> g2ToG3 >>> g3ToRight1, bToG2 >>> g2ToG3 >>> g3ToRight2
            , g2ToG1 >>> g1ToA, g2ToB, g2ToC
                , g2ToG3 >>> g3ToD
            , cToG2 >>> g2ToG1 >>> g1ToLeft1, cToG2 >>> g2ToG1 >>> g1ToLeft2, cToG2 >>> g2ToG1
                , cToG2, cToG3, cToG3>>> g3ToRight1, cToG3>>> g3ToRight2
            , g3ToG2 >>> g2ToG1 >>> g1ToA, g3ToG2 >>> g2ToB, g3ToC
                , g3ToD
            , dToG3 >>> g3ToG2 >>> g2ToG1 >>> g1ToLeft1, dToG3 >>> g3ToG2 >>> g2ToG1 >>> g1ToLeft2, dToG3 >>> g3ToG2 >>> g2ToG1
                , dToG3 >>> g3ToG2, dToG3, dToRight1, dToRight2
            , right1ToG3 >>> g3ToG2 >>> g2ToG1 >>> g1ToA, right1ToG3 >>> g3ToG2 >>> g2ToB, right1ToG3 >>> g3ToC
                , right1ToD
            , right2ToG3 >>> g3ToG2 >>> g2ToG1 >>> g1ToA, right2ToG3 >>> g3ToG2 >>> g2ToB, right2ToG3 >>> g3ToC
                , right2ToD]

    addCost c (Node x d) = Node x (d+c)
    addPath n = WithPath (n, (n:path))





--  user specifies a function to get the adjacent 
dijk :: (Node ->  PQ Node) -> PQ Node -> Set Burrow -> Burrow -> Maybe Int
dijk getAdj pq visited end = do
    (curr@(Node coord dist), pq') <- PQ.minView pq
    -- let dist' = 
    let visited' = Set.insert coord visited


    if coord == end 
      then Just dist
      else if coord `Set.member` visited
        then dijk getAdj pq' visited end
        else dijk getAdj (PQ.union pq' $ getAdj curr) visited' end


-- also returns the path
dijkWithPath :: (WithPath Node ->  PQ (WithPath Node)) -> PQ (WithPath Node) -> Set Burrow -> Burrow -> Maybe (Int, [Node])
dijkWithPath getAdj pq visited end = do
    (curr@(WithPath (Node coord dist, path)), pq') <- PQ.minView pq
    -- let dist' = 
    let visited' = Set.insert coord visited

    let adjacencies = PQ.filter (\(WithPath (Node x _, _)) -> not $ x `Set.member` visited)  $ getAdj curr
    -- adjacencies = getAdj curr

    if coord == end 
      then Just (dist, path)
      else if coord `Set.member` visited
        then dijkWithPath getAdj pq' visited end
        else dijkWithPath getAdj (PQ.union pq' adjacencies) visited' end




-- Move  = Burrow -> Maybe Node
(>>>) :: Move -> Move -> Move
(>>>) m1 m2 = \burrow -> do
  (Node burrow' cost1) <- m1 burrow
  (Node burrow'' cost2) <- m2 burrow'
  Just $ Node burrow'' (cost1 + cost2)


blockBy :: Maybe a -> Maybe ()
blockBy Nothing = Just ()
blockBy _ = Nothing





-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


left1ToA :: Move
left1ToA = \burrow -> do
  pod <- left1 burrow
  when (pod /= A) Nothing
  when (not $ all (==A) $ roomA burrow) Nothing
  blockBy $ left2 burrow
  
  let cost = stepCost pod * (4 - length (roomA burrow))
  let burrow' = burrow
                { left1 = Nothing
                , roomA = pod:(roomA burrow)
                }
  return $ Node burrow' cost

left2ToA :: Move
left2ToA = \burrow -> do
  pod <- left2 burrow
  when (pod /= A) Nothing
  when (not $ all (==A) $ roomA burrow) Nothing
  
  let cost = stepCost pod * (3 - length (roomA burrow))
  let burrow' = burrow
                { left2 = Nothing 
                , roomA = pod:(roomA burrow)
                }
  return $ Node burrow' cost

left1ToG1 :: Burrow -> Maybe Node
left1ToG1 burrow = do
  pod <- left1 burrow
  when (not $ null $ gap1 burrow) Nothing
  blockBy $ left2 burrow
  
  let cost = stepCost pod * 3
  let burrow' = burrow
                { left1 = Nothing
                , gap1 = Just pod
                }
  return $ Node burrow' cost

left2ToG1 :: Burrow -> Maybe Node
left2ToG1 burrow = do
  pod <- left2 burrow
  blockBy $ gap1 burrow
  
  let cost = stepCost pod * 2
  let burrow' = burrow
                { left2 = Nothing
                , gap1 = Just pod
                }
  return $ Node burrow' cost

aToG1 :: Burrow -> Maybe Node
aToG1 burrow = do
  pod <- safeHead $ roomA burrow
  blockBy $ gap1 burrow
  
  let cost = stepCost pod * (4 - length (roomA burrow))
  let burrow' = burrow
                { roomA = (tail $ roomA burrow) 
                , gap1 = Just pod
                }
  return $ Node burrow' cost











g1ToB :: Move
g1ToB = \burrow -> do
  pod <- gap1 burrow
  when (pod /= B) Nothing
  when (not $ all (==B) $ roomB burrow) Nothing

  let cost = stepCost pod * (3 - length (roomB burrow))
  let burrow' = burrow
                { gap1 = Nothing
                , roomB = pod:(roomB burrow)
                }
  return $ Node burrow' cost

g1ToG2 :: Move
g1ToG2 = \burrow -> do
  pod <- gap1 burrow
  blockBy $ gap2 burrow

  let cost = stepCost pod * 2
  let burrow' = burrow
                { gap1 = Nothing
                , gap2 = Just pod
                }
  return $ Node burrow' cost


bToG2 :: Burrow -> Maybe Node
bToG2 burrow = do
  pod <- safeHead $ roomB burrow
  blockBy $ gap2 burrow
  
  let cost = stepCost pod * (4 - length (roomB burrow))
  let burrow' = burrow
                { roomB = (tail $ roomB burrow) 
                , gap2 = Just pod
                }
  return $ Node burrow' cost












g2ToC :: Move
g2ToC = \burrow -> do
  pod <- gap2 burrow
  when (pod /= C) Nothing
  when (not $ all (==C) $ roomC burrow) Nothing

  let cost = stepCost pod * (3 - length (roomC burrow))
  let burrow' = burrow
                { gap2 = Nothing
                , roomC = pod:(roomC burrow)
                }
  return $ Node burrow' cost

g2ToG3 :: Move
g2ToG3 = \burrow -> do
  pod <- gap2 burrow
  blockBy $ gap3 burrow

  let cost = stepCost pod * 2
  let burrow' = burrow
                { gap2 = Nothing
                , gap3 = Just pod
                }
  return $ Node burrow' cost


cToG3 :: Burrow -> Maybe Node
cToG3 burrow = do
  pod <- safeHead $ roomC burrow
  blockBy $ gap3 burrow
  
  let cost = stepCost pod * (4 - length (roomC burrow))
  let burrow' = burrow
                { roomC = (tail $ roomC burrow) 
                , gap3 = Just pod
                }
  return $ Node burrow' cost











g3ToD :: Move
g3ToD = \burrow -> do
  pod <- gap3 burrow
  when (pod /= D) Nothing
  when (not $ all (==D) $ roomD burrow) Nothing

  let cost = stepCost pod * (3 - length (roomD burrow))
  let burrow' = burrow
                { gap3 = Nothing
                , roomD = pod:(roomD burrow)
                }
  return $ Node burrow' cost

g3ToRight1 :: Move
g3ToRight1 = \burrow -> do
  pod <- gap3 burrow
  blockBy $ right1 burrow
  blockBy $ right2 burrow

  let cost = stepCost pod * 3
  let burrow' = burrow
                { gap3 = Nothing
                , right1 = Just pod
                }
  return $ Node burrow' cost

g3ToRight2 :: Move
g3ToRight2 = \burrow -> do
  pod <- gap3 burrow
  blockBy $ right2 burrow

  let cost = stepCost pod * 2
  let burrow' = burrow
                { gap3 = Nothing
                , right2 = Just pod
                }
  return $ Node burrow' cost

dToRight1 :: Burrow -> Maybe Node
dToRight1 burrow = do
  pod <- safeHead $ roomD burrow
  blockBy $ right1 burrow
  blockBy $ right2 burrow
  
  let cost = stepCost pod * (5 - length (roomD burrow))
  let burrow' = burrow
                { roomD = (tail $ roomD burrow) 
                , right1 = Just pod
                }
  return $ Node burrow' cost


dToRight2 :: Burrow -> Maybe Node
dToRight2 burrow = do
  pod <- safeHead $ roomD burrow
  blockBy $ right2 burrow
  
  let cost = stepCost pod * (4 - length (roomD burrow))
  let burrow' = burrow
                { roomD = (tail $ roomD burrow) 
                , right2 = Just pod
                }
  return $ Node burrow' cost


-- <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

right1ToD :: Move
right1ToD = \burrow -> do
  pod <- right1 burrow
  blockBy $ right2 burrow
  when (pod /= D) Nothing
  when (not $ all (==D) $ roomD burrow) Nothing

  let cost = stepCost pod * (4 - length (roomD burrow))
  let burrow' = burrow
                { roomD = pod:(roomD burrow)
                , right1 = Nothing
                }
  return $ Node burrow' cost

right2ToD :: Move
right2ToD = \burrow -> do
  pod <- right2 burrow

  when (pod /= D) Nothing
  when (not $ all (==D) $ roomD burrow) Nothing

  let cost = stepCost pod * (3 - length (roomD burrow))
  let burrow' = burrow
                { roomD = pod:(roomD burrow)
                , right2 = Nothing
                }
  return $ Node burrow' cost


right1ToG3 :: Move
right1ToG3 = \burrow -> do
  pod <- right1 burrow

  blockBy $ right2 burrow
  blockBy $ gap3 burrow

  let cost = stepCost pod * 3
  let burrow' = burrow
                { gap3 = Just pod
                , right1 = Nothing
                }
  return $ Node burrow' cost

right2ToG3 :: Move
right2ToG3 = \burrow -> do
  pod <- right2 burrow
  blockBy $ gap3 burrow

  let cost = stepCost pod * 2
  let burrow' = burrow
                { gap3 = Just pod
                , right2 = Nothing
                }
  return $ Node burrow' cost


dToG3 :: Burrow -> Maybe Node
dToG3 burrow = do
  pod <- safeHead $ roomD burrow
  blockBy $ gap3 burrow
  
  let cost = stepCost pod * (4 - length (roomD burrow))
  let burrow' = burrow
                { roomD = (tail $ roomD burrow) 
                , gap3 = Just pod
                }
  return $ Node burrow' cost











g3ToC :: Move
g3ToC = \burrow -> do
  pod <- gap3 burrow
  when (pod /= C) Nothing
  when (not $ all (==C) $ roomC burrow) Nothing

  let cost = stepCost pod * (3 - length (roomC burrow))
  let burrow' = burrow
                { gap3 = Nothing
                , roomC = pod:(roomC burrow)
                }
  return $ Node burrow' cost

g3ToG2 :: Move
g3ToG2 = \burrow -> do
  pod <- gap3 burrow
  blockBy $ gap2 burrow

  let cost = stepCost pod * 2
  let burrow' = burrow
                { gap3 = Nothing
                , gap2 = Just pod
                }
  return $ Node burrow' cost

cToG2 :: Burrow -> Maybe Node
cToG2 burrow = do
  pod <- safeHead $ roomC burrow
  blockBy $ gap2 burrow
  
  let cost = stepCost pod * (4 - length (roomC burrow))
  let burrow' = burrow
                { roomC = (tail $ roomC burrow) 
                , gap2 = Just pod
                }
  return $ Node burrow' cost









g2ToB :: Move
g2ToB = \burrow -> do
  pod <- gap2 burrow
  when (pod /= B) Nothing
  when (not $ all (==B) $ roomB burrow) Nothing

  let cost = stepCost pod * (3 - length (roomB burrow))
  let burrow' = burrow
                { gap2 = Nothing
                , roomB = pod:(roomB burrow)
                }
  return $ Node burrow' cost

g2ToG1 :: Move
g2ToG1 = \burrow -> do
  pod <- gap2 burrow
  blockBy $ gap1 burrow

  let cost = stepCost pod * 2
  let burrow' = burrow
                { gap2 = Nothing
                , gap1 = Just pod
                }
  return $ Node burrow' cost

bToG1 :: Burrow -> Maybe Node
bToG1 burrow = do
  pod <- safeHead $ roomB burrow
  blockBy $ gap1 burrow
  
  let cost = stepCost pod * (4 - length (roomB burrow))
  let burrow' = burrow
                { roomB = (tail $ roomB burrow) 
                , gap1 = Just pod
                }
  return $ Node burrow' cost











g1ToA :: Move
g1ToA = \burrow -> do
  pod <- gap1 burrow
  when (pod /= A) Nothing
  when (not $ all (==A) $ roomA burrow) Nothing

  let cost = stepCost pod * (3 - length (roomA burrow))
  let burrow' = burrow
                { gap1 = Nothing
                , roomA = pod:(roomA burrow)
                }
  return $ Node burrow' cost



g1ToLeft1 :: Burrow -> Maybe Node
g1ToLeft1 burrow = do
  pod <- gap1 burrow
  blockBy $ left1 burrow
  blockBy $ left2 burrow
  
  let cost = stepCost pod * 3
  let burrow' = burrow
                { gap1 = Nothing
                , left1 = Just pod
                }
  return $ Node burrow' cost


g1ToLeft2 :: Burrow -> Maybe Node
g1ToLeft2 burrow = do
  pod <- gap1 burrow
  when (left2 burrow /= Nothing) Nothing
  
  let cost = stepCost pod * 2
  let burrow' = burrow
                { gap1 = Nothing
                , left2 = Just pod
                }
  return $ Node burrow' cost

aToLeft1 :: Move
aToLeft1 = \burrow -> do
  pod <- safeHead $ roomA burrow
  blockBy $ left1 burrow
  blockBy $ left2 burrow 
  
  let cost = stepCost pod * (5 - length (roomA burrow))
  let burrow' = burrow
                { roomA = tail $ roomA burrow
                , left1 = Just pod
                }
  return $ Node burrow' cost

aToLeft2 :: Move
aToLeft2 = \burrow -> do
  pod <- safeHead $ roomA burrow
  blockBy $ left2 burrow
  
  let cost = stepCost pod * (4 - length (roomA burrow))
  let burrow' = burrow
                { roomA = tail $ roomA burrow
                , left2 = Just pod
                }
  return $ Node burrow' cost





































-- ========================================================



safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x