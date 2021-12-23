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
  left :: [Pod],
  roomA :: [Pod],
  gap1 :: [Pod],
  roomB :: [Pod],
  gap2 :: [Pod],
  roomC :: [Pod],
  gap3 :: [Pod],
  roomD :: [Pod],
  right :: [Pod]
}
  deriving Eq 
instance Hashable Burrow where
  hashWithSalt salt (Burrow  l a g1 b g2 c g3 d r) = hashWithSalt salt [l, a, g1, b, g2, c, g3, d, r]
instance Show Burrow where
  show (Burrow  l a g1 b g2 c g3 d r) = 
      "#############\n"
    ++"#" ++ showLeft l ++ "." ++ showGap g1 ++ "." ++ showGap g2 ++ "." ++ showGap g3 ++ "." ++ showRight r ++ "#\n"
    ++"###" ++ secondPod a ++ "#" ++ secondPod b ++ "#" ++ secondPod c ++ "#" ++ secondPod d ++ "###\n"
    ++"  #" ++ firstPod a ++ "#" ++ firstPod b ++ "#" ++ firstPod c ++ "#" ++ firstPod d ++ "#  \n"
    ++"  #########"

showGap :: [Pod] -> String
showGap [] = "."
showGap (x:xs) = show x

firstPod [] = "."
firstPod xs = show $ last xs

secondPod (x:_:_) = show x
secondPod _ = "."

showLeft [] = ".."
showLeft [x] = show x ++ "."
showLeft (x:y:xs) = show x ++ show y

showRight [] = ".."
showRight [x] = "." ++ show x
showRight (x:y:xs) = show y ++ show x



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
  let test = Burrow 
                { left = []
                , roomA =[B, A]
                , gap1 =[]
                , roomB =[C, D]
                , gap2 =[]
                , roomC =[B, C]
                , gap3 =[]
                , roomD =[D, A]
                , right = []
                }

  let input = Burrow 
                { left = []
                , roomA =[A, D]
                , gap1 =[]
                , roomB =[C, A]
                , gap2 =[]
                , roomC =[B, D]
                , gap3 =[]
                , roomD =[C, B]
                , right = []
                }

  putStr "Part 1: "
  let result1 = solve1 test
  case result1 of
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
            { left = []
            , roomA =[A, A]
            , gap1 =[]
            , roomB =[B, B]
            , gap2 =[]
            , roomC =[C, C]
            , gap3 =[]
            , roomD =[D, D]
            , right = []
            }








options :: Node -> PQ Node
options (Node curr dist) = result
  where
    result = PQ.fromList $ map (addCost dist) adj

    -- [Node]
    adj = catMaybes $ map ($curr) moves
    moves = [leftToA, leftToG1 >>> g1ToB, leftToG1 >>> g1ToG2 >>> g2ToC, leftToG1 >>> g1ToG2 >>> g2ToG3 >>> g3ToD
            , aToLeft, aToG1, aToG1 >>> g1ToG2, aToG1 >>> g1ToG2 >>> g2ToG3, aToG1 >>> g1ToG2 >>> g2ToG3 >> g3ToRight
            , g1ToA, g1ToB, g1ToG2 >>> g2ToC, g1ToG2 >>> g2ToG3 >>> g3ToD
            , bToG1 >>> g1ToLeft, bToG1, bToG2, bToG2 >>> g2ToG3, bToG2 >>> g2ToG3 >>> g3ToRight
            , g2ToG1 >>> g1ToA, g2ToB, g2ToC, g2ToG3 >>> g3ToD
            , cToG2 >>> g2ToG1 >>> g1ToLeft, cToG2 >>> g2ToG1, cToG2, cToG3, cToG3>>> g3ToRight
            , g3ToG2 >>> g2ToG1 >>> g1ToLeft, g3ToG2 >>> g2ToG1 >>> g1ToA, g3ToG2 >>> g2ToB, g3ToC, g3ToD
            , dToG3 >>> g3ToG2 >>> g2ToG1 >>> g1ToLeft, dToG3 >>> g3ToG2 >>> g2ToG1, dToG3 >>> g3ToG2, dToG3, dToRight
            , rightToG3 >>> g3ToG2 >>> g2ToG1 >>> g1ToA, rightToG3 >>> g3ToG2 >>> g2ToB, rightToG3 >>> g3ToC, rightToD]

    addCost c (Node x d) = Node x (d+c)



optionsWithPath :: WithPath Node -> PQ (WithPath Node)
optionsWithPath (WithPath ((Node curr dist), path)) = result
  where
    result = PQ.fromList $ map (addPath . addCost dist) adj

    -- [Node]
    adj = catMaybes $ map ($curr) moves
    moves = [leftToA, leftToG1 >>> g1ToB, leftToG1 >>> g1ToG2 >>> g2ToC, leftToG1 >>> g1ToG2 >>> g2ToG3 >>> g3ToD
            , aToLeft, aToG1, aToG1 >>> g1ToG2, aToG1 >>> g1ToG2 >>> g2ToG3, aToG1 >>> g1ToG2 >>> g2ToG3 >> g3ToRight
            , g1ToA, g1ToB, g1ToG2 >>> g2ToC, g1ToG2 >>> g2ToG3 >>> g3ToD
            , bToG1 >>> g1ToLeft, bToG1, bToG2, bToG2 >>> g2ToG3, bToG2 >>> g2ToG3 >>> g3ToRight
            , g2ToG1 >>> g1ToA, g2ToB, g2ToC, g2ToG3 >>> g3ToD
            , cToG2 >>> g2ToG1 >>> g1ToLeft, cToG2 >>> g2ToG1, cToG2, cToG3, cToG3>>> g3ToRight
            , g3ToG2 >>> g2ToG1 >>> g1ToLeft, g3ToG2 >>> g2ToG1 >>> g1ToA, g3ToG2 >>> g2ToB, g3ToC, g3ToD
            , dToG3 >>> g3ToG2 >>> g2ToG1 >>> g1ToLeft, dToG3 >>> g3ToG2 >>> g2ToG1, dToG3 >>> g3ToG2, dToG3, dToRight
            , rightToG3 >>> g3ToG2 >>> g2ToG1 >>> g1ToA, rightToG3 >>> g3ToG2 >>> g2ToB, rightToG3 >>> g3ToC, rightToD]

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

    adjacencies = PQ.filter (\(WithPath (Node x _, _)) -> not $ x `Set.member` visited)  $ getAdj curr
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








-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


leftToA :: Move
leftToA = \burrow -> do
  pod <- safeHead $ left burrow
  when (pod /= A) Nothing
  when (not $ all (==A) $ roomA burrow) Nothing
  
  let cost = stepCost pod * (5 - length (left burrow) - length (roomA burrow))
  let burrow' = burrow
                { left = (tail $ left burrow) 
                , roomA = pod:(roomA burrow)
                }
  return $ Node burrow' cost


leftToG1 :: Burrow -> Maybe Node
leftToG1 burrow = do
  pod <- safeHead $ left burrow
  when (not $ null $ gap1 burrow) Nothing
  
  let cost = stepCost pod * (4 - length (left burrow))
  let burrow' = burrow
                { left = (tail $ left burrow) 
                , gap1 = pod:(gap1 burrow)
                }
  return $ Node burrow' cost

aToG1 :: Burrow -> Maybe Node
aToG1 burrow = do
  pod <- safeHead $ roomA burrow
  when (not $ null $ gap1 burrow) Nothing
  
  let cost = stepCost pod * (4 - length (roomA burrow))
  let burrow' = burrow
                { roomA = (tail $ roomA burrow) 
                , gap1 = pod:(gap1 burrow)
                }
  return $ Node burrow' cost











g1ToB :: Move
g1ToB = \burrow -> do
  pod <- safeHead $ gap1 burrow
  when (pod /= B) Nothing
  when (not $ all (==B) $ roomB burrow) Nothing

  let cost = stepCost pod * (3 - length (roomB burrow))
  let burrow' = burrow
                { gap1 = []
                , roomB = pod:(roomB burrow)
                }
  return $ Node burrow' cost

g1ToG2 :: Move
g1ToG2 = \burrow -> do
  pod <- safeHead $ gap1 burrow
  when (not $ null $ gap2 burrow) Nothing

  let cost = stepCost pod * 2
  let burrow' = burrow
                { gap1 = []
                , gap2 = [pod]
                }
  return $ Node burrow' cost


bToG2 :: Burrow -> Maybe Node
bToG2 burrow = do
  pod <- safeHead $ roomB burrow
  when (not $ null $ gap2 burrow) Nothing
  
  let cost = stepCost pod * (4 - length (roomB burrow))
  let burrow' = burrow
                { roomB = (tail $ roomB burrow) 
                , gap2 = pod:(gap2 burrow)
                }
  return $ Node burrow' cost












g2ToC :: Move
g2ToC = \burrow -> do
  pod <- safeHead $ gap2 burrow
  when (pod /= C) Nothing
  when (not $ all (==C) $ roomC burrow) Nothing

  let cost = stepCost pod * (3 - length (roomC burrow))
  let burrow' = burrow
                { gap2 = []
                , roomC = pod:(roomC burrow)
                }
  return $ Node burrow' cost

g2ToG3 :: Move
g2ToG3 = \burrow -> do
  pod <- safeHead $ gap2 burrow
  when (not $ null $ gap3 burrow) Nothing

  let cost = stepCost pod * 2
  let burrow' = burrow
                { gap2 = []
                , gap3 = [pod]
                }
  return $ Node burrow' cost


cToG3 :: Burrow -> Maybe Node
cToG3 burrow = do
  pod <- safeHead $ roomC burrow
  when (not $ null $ gap3 burrow) Nothing
  
  let cost = stepCost pod * (4 - length (roomC burrow))
  let burrow' = burrow
                { roomC = (tail $ roomC burrow) 
                , gap3 = pod:(gap3 burrow)
                }
  return $ Node burrow' cost











g3ToD :: Move
g3ToD = \burrow -> do
  pod <- safeHead $ gap3 burrow
  when (pod /= D) Nothing
  when (not $ all (==D) $ roomD burrow) Nothing

  let cost = stepCost pod * (3 - length (roomD burrow))
  let burrow' = burrow
                { gap3 = []
                , roomD = pod:(roomD burrow)
                }
  return $ Node burrow' cost

g3ToRight :: Move
g3ToRight = \burrow -> do
  pod <- safeHead $ gap3 burrow
  when ((>=2) $ length $ right burrow) Nothing

  let cost = stepCost pod * (3 - length (right burrow))
  let burrow' = burrow
                { gap3 = []
                , right = pod:(right burrow)
                }
  return $ Node burrow' cost

dToRight :: Burrow -> Maybe Node
dToRight burrow = do
  pod <- safeHead $ roomD burrow
  when ((>=2) $ length $ right burrow) Nothing
  
  let cost = stepCost pod * (5 - length (roomD burrow) - length (right burrow))
  let burrow' = burrow
                { roomD = (tail $ roomD burrow) 
                , right = pod:(right burrow)
                }
  return $ Node burrow' cost



-- <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

rightToD :: Move
rightToD = \burrow -> do
  pod <- safeHead $ right burrow
  when (pod /= D) Nothing
  when (not $ all (==D) $ roomD burrow) Nothing

  let cost = stepCost pod * (5 - length (right burrow) - length (roomD burrow))
  let burrow' = burrow
                { roomD = pod:(roomD burrow)
                , right = tail (right burrow)
                }
  return $ Node burrow' cost


rightToG3 :: Move
rightToG3 = \burrow -> do
  pod <- safeHead $ right burrow
  when (not $ null $ gap3 burrow) Nothing

  let cost = stepCost pod * (4 - length (right burrow))
  let burrow' = burrow
                { gap3 = [pod]
                , right = tail (right burrow)
                }
  return $ Node burrow' cost


dToG3 :: Burrow -> Maybe Node
dToG3 burrow = do
  pod <- safeHead $ roomD burrow
  when (not $ null $ gap3 burrow) Nothing
  
  let cost = stepCost pod * (4 - length (roomD burrow))
  let burrow' = burrow
                { roomD = (tail $ roomD burrow) 
                , gap3 = pod:(gap3 burrow)
                }
  return $ Node burrow' cost











g3ToC :: Move
g3ToC = \burrow -> do
  pod <- safeHead $ gap3 burrow
  when (pod /= C) Nothing
  when (not $ all (==C) $ roomC burrow) Nothing

  let cost = stepCost pod * (3 - length (roomC burrow))
  let burrow' = burrow
                { gap3 = []
                , roomC = pod:(roomC burrow)
                }
  return $ Node burrow' cost

g3ToG2 :: Move
g3ToG2 = \burrow -> do
  pod <- safeHead $ gap3 burrow
  when (not $ null $ gap2 burrow) Nothing

  let cost = stepCost pod * 2
  let burrow' = burrow
                { gap3 = []
                , gap2 = [pod]
                }
  return $ Node burrow' cost

cToG2 :: Burrow -> Maybe Node
cToG2 burrow = do
  pod <- safeHead $ roomC burrow
  when (not $ null $ gap2 burrow) Nothing
  
  let cost = stepCost pod * (4 - length (roomC burrow))
  let burrow' = burrow
                { roomC = (tail $ roomC burrow) 
                , gap2 = pod:(gap2 burrow)
                }
  return $ Node burrow' cost









g2ToB :: Move
g2ToB = \burrow -> do
  pod <- safeHead $ gap2 burrow
  when (pod /= B) Nothing
  when (not $ all (==B) $ roomB burrow) Nothing

  let cost = stepCost pod * (3 - length (roomB burrow))
  let burrow' = burrow
                { gap2 = []
                , roomB = pod:(roomB burrow)
                }
  return $ Node burrow' cost

g2ToG1 :: Move
g2ToG1 = \burrow -> do
  pod <- safeHead $ gap2 burrow
  when (not $ null $ gap1 burrow) Nothing

  let cost = stepCost pod * 2
  let burrow' = burrow
                { gap2 = []
                , gap1 = [pod]
                }
  return $ Node burrow' cost

bToG1 :: Burrow -> Maybe Node
bToG1 burrow = do
  pod <- safeHead $ roomB burrow
  when (not $ null $ gap1 burrow) Nothing
  
  let cost = stepCost pod * (4 - length (roomB burrow))
  let burrow' = burrow
                { roomB = (tail $ roomB burrow) 
                , gap1 = pod:(gap1 burrow)
                }
  return $ Node burrow' cost











g1ToA :: Move
g1ToA = \burrow -> do
  pod <- safeHead $ gap1 burrow
  when (pod /= A) Nothing
  when (not $ all (==A) $ roomA burrow) Nothing

  let cost = stepCost pod * (3 - length (roomA burrow))
  let burrow' = burrow
                { gap1 = []
                , roomA = pod:(roomA burrow)
                }
  return $ Node burrow' cost

g1ToLeft :: Burrow -> Maybe Node
g1ToLeft burrow = do
  pod <- safeHead $ gap1 burrow
  when ((>=2) $ length $ left burrow) Nothing
  
  let cost = stepCost pod * (3 - length (left burrow))
  let burrow' = burrow
                { gap1 = (tail $ gap1 burrow) 
                , left = pod:(left burrow)
                }
  return $ Node burrow' cost

aToLeft :: Move
aToLeft = \burrow -> do
  pod <- safeHead $ roomA burrow
  when ((>=2) $ length $ left burrow) Nothing
  
  let cost = stepCost pod * (5 - length (left burrow) - length (roomA burrow))
  let burrow' = burrow
                { roomA = tail $ roomA burrow
                , left = pod:(left burrow) 
                }
  return $ Node burrow' cost





































-- ========================================================



safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x