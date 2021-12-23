{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.List (foldl')
import qualified Data.HashSet as Set
import qualified Data.PQueue.Min as PQ
import qualified Data.Vector as V
import Data.Vector ((!))
-- import Helpers (chain)
import Data.Maybe
import Control.Monad
import Data.Hashable
import ThiccIO
import Control.Monad.IO.Class


import Debug.Trace
ttrace x = trace (show x) x




type Set = Set.HashSet
type PQ = PQ.MinQueue
type V = V.Vector



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




data Spot = Spot (Maybe Pod) | Room Pod [Pod]
  deriving Eq
instance Hashable Spot where
  hashWithSalt s (Spot x) = hashWithSalt s x
  hashWithSalt s (Room t xs) = hashWithSalt s (t:xs)
-- instance Show Spot where
--   show 


type Burrow = V Spot
instance Hashable Burrow where
  hashWithSalt s b = hashWithSalt s $ V.toList b

printBurrow :: Burrow -> IO ()
printBurrow b = runThicc $ 
  do
    liftIO $ putStrLn "#############"
    putTall $ "######"
    putTall $ tallSpot $ b ! 0
    putTall $ tallSpot $ b ! 1
    putTall $ tallSpot $ b ! 2
    putTall $ tallSpot $ b ! 3
    putTall $ tallSpot $ b ! 4
    putTall $ tallSpot $ b ! 5
    putTall $ tallSpot $ b ! 6
    putTall $ tallSpot $ b ! 7
    putTall $ tallSpot $ b ! 8
    putTall $ tallSpot $ b ! 9
    putTall $ tallSpot $ b ! 10
    putTall $ "######"


tallSpot :: Spot -> String
tallSpot (Spot Nothing) = ".#####"
tallSpot (Spot (Just x)) = show x ++ "#####"
tallSpot (Room _ xs) = "." ++ (take (4 - length xs) "....") ++ concatMap show xs ++ "#"



data Node = Node Burrow Int
instance Ord Node where
  compare (Node _ risk1) (Node _ risk2) = compare risk1 risk2
instance Eq Node where
  (==) (Node b1 c1) (Node b2 c2) = (b1 == b2) && (c1 == c2)
instance Hashable Node where
  hashWithSalt salt (Node b c) = hashWithSalt salt (b,c)
-- instance Show Node where
--   show (Node burrow cost) = "\n" ++ show burrow ++ " " ++ show cost ++ "\n\n"



newtype WithPath a = WithPath (a, [a])
instance Ord a => Ord (WithPath a) where
  compare (WithPath (n1, _)) (WithPath (n2, _)) = compare n1 n2
instance Eq a => Eq (WithPath a) where
  (==) (WithPath (n1, _)) (WithPath (n2, _)) = (==) n1 n2
instance Hashable a => Hashable (WithPath a) where
  hashWithSalt salt (WithPath (n, path))= hashWithSalt salt (n,path)
instance Show a => Show (WithPath a) where
  show (WithPath (n,_)) = show n



type Move = Burrow -> Maybe Node



part1 = True

main = do
  let test = V.fromList [ Spot Nothing
              , Spot Nothing
              , Room A [B, A]
              , Spot Nothing
              , Room B [C, D]
              , Spot Nothing
              , Room C [B, C]
              , Spot Nothing
              , Room D [D, A]
              , Spot Nothing
              , Spot Nothing
              ]

  let input = V.fromList [ Spot Nothing
              , Spot Nothing
              , Room A [A, D]
              , Spot Nothing
              , Room B [C, A]
              , Spot Nothing
              , Room C [B, D]
              , Spot Nothing
              , Room D [C, B]
              , Spot Nothing
              , Spot Nothing
              ]
  -- putStr "Part 1: "
  -- let result1 = solve1 test1
  -- case result1 of
  --   Nothing -> putStrLn "Couldn't find a solution"
  --   Just (dist, path) -> do
  --     mapM (printBurrow . (\(Node b d) -> b)) $ reverse path
  --     print dist

  putStrLn "Part 2: "
  let result2 = solve2 test
  case result2 of
    Nothing -> putStrLn "Couldn't find a solution"
    Just (dist, path) -> do
      mapM (printBurrow . (\(Node b d) -> b)) $ reverse path
      print dist



solve1 :: Burrow -> Maybe (Int, [Node])
solve1 burrow = result
  where
    result = dijkWithPath optionsWithPath pq visited end

    visited = Set.empty
    pq = optionsWithPath $ WithPath (start, [start])
    start = (Node burrow 0)
    end = V.fromList [ Spot Nothing
          , Spot Nothing
          , Room A [A, A]
          , Spot Nothing
          , Room B [B, B]
          , Spot Nothing
          , Room C [C, C]
          , Spot Nothing
          , Room D [D, D]
          , Spot Nothing
          , Spot Nothing
          ]



inject [a,b] (Room t (x:xs)) = Room t $ [x, a, b] ++ xs

preprocess2 :: Burrow -> Burrow 
preprocess2 burrow =  mapAt 2 (inject [D, D]) $ 
                      mapAt 4 (inject [C, B]) $ 
                      mapAt 6 (inject [B, A]) $ 
                      mapAt 8 (inject [A, C]) $
                      burrow

solve2 :: Burrow -> Maybe (Int, [Node])
solve2 burrow = result
  where
    result = dijkWithPath optionsWithPath pq visited end

    visited = Set.empty
    pq = optionsWithPath $ WithPath (start, [start])

    start = (Node (preprocess2 burrow) 0)
    end = V.fromList [ Spot Nothing
          , Spot Nothing
          , Room A [A, A, A, A]
          , Spot Nothing
          , Room B [B, B, B, B]
          , Spot Nothing
          , Room C [C, C, C, C]
          , Spot Nothing
          , Room D [D, D, D, D]
          , Spot Nothing
          , Spot Nothing
          ]

optionsWithPath :: WithPath Node -> PQ (WithPath Node)
optionsWithPath (WithPath ((Node curr dist), path)) = result
  where

    result = PQ.fromList $ map (addPath . addCost dist) adj

    -- [Node]12
    adj = catMaybes $ map ($curr) $ moves 
    n = length curr
    moves = [move i j | i <- spots, j <- rooms] ++ [move j i | i <- spots, j <- rooms]
    spots = [0, 1, 3, 5, 7, 9]
    rooms = [2, 4, 6, 8]

    addCost c (Node x d) = Node x (d+c)
    addPath n = WithPath (n, (n:path))


-- also returns the path
dijkWithPath :: (WithPath Node ->  PQ (WithPath Node)) -> PQ (WithPath Node) -> Set Burrow -> Burrow -> Maybe (Int, [Node])
dijkWithPath getAdj pq visited end = do
    (curr@(WithPath (Node coord dist, path)), pq') <- PQ.minView pq
    -- let dist' = 
    let visited' = Set.insert coord visited

    -- let adjacencies = PQ.filter (\(WithPath (Node x _, _)) -> not $ x `Set.member` visited)  $ getAdj curr
    let adjacencies = getAdj curr

    if coord == end 
      then Just (dist, path)
      else if coord `Set.member` visited
        then dijkWithPath getAdj pq' visited end
        else dijkWithPath getAdj (PQ.union pq' adjacencies) visited' end

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



validMove :: Spot -> Spot -> Bool
validMove (Spot _) (Room _ _) = True
validMove (Room _ _) (Spot _) = True
validMove _ _ = False

assert False = Nothing
assert True = Just ()

slice :: Int -> Int -> [a] -> [a]
slice s e xs = take (e-s+1) $ drop s xs

clearBetween :: Int -> Int -> Burrow -> Bool
clearBetween start end burrow = all clear $ V.take (hi-lo-1) $ V.drop (lo+1) burrow
  where
    (lo, hi) = (min start end, max start end)
     

clear :: Spot -> Bool
clear (Spot Nothing) = True
clear (Room _ _) = True
clear _ = False

removePod :: Spot -> Spot
removePod (Spot _) = Spot Nothing
removePod (Room target (_:xs)) = Room target xs

addPod :: Pod -> Spot -> Spot
addPod x (Spot _) = Spot (Just x)
addPod x (Room target xs) = Room target (x:xs)

-- very partial
setAt :: Int -> a -> V a -> V a
setAt n x xs = V.take n xs <> (V.singleton x) <> V.drop (n+1) xs

mapAt :: Int -> (a -> a) -> V a -> V a
mapAt n f xs = V.take n xs <> V.singleton (f (xs ! n)) <> V.drop (n+1) xs

firstPod :: Spot -> Maybe Pod
firstPod (Spot (Just x)) = Just x
firstPod (Room _ (x:_)) = Just x
firstPod _ = Nothing


podMatches :: Pod -> Spot -> Bool
podMatches x (Spot _) = True
podMatches x (Room t _) = t == x

podsMatch :: Spot -> Bool
podsMatch (Spot _) = True
podsMatch (Room t xs) = all (==t) xs

fillness :: Spot -> Int
fillness (Spot Nothing) = 0
fillness (Spot _) = 1
fillness (Room _ xs) = length xs

move :: Int -> Int -> Burrow -> Maybe Node
move start end burrow = do
  assert $ start < length burrow
  assert $ end < length burrow
  assert $ validMove (burrow ! start) (burrow ! end)
  assert $ clearBetween start end burrow

  pod <- firstPod $ burrow ! start
  let entering = case burrow ! start of
                  Spot _ -> True
                  _ -> False


  -- if it's a room, make sure the pod we're moving matches
  --    and all the pods currently in there also match
  when entering $ assert $ podsMatch (burrow ! end) && podMatches pod (burrow ! end)


  let burrow' = mapAt start (removePod) $ mapAt end (addPod pod) $ burrow
  let cost = if entering
              then (abs $ end - start) + (4 - fillness (burrow ! end))
              else (abs $ end - start) + (5 - fillness (burrow ! start))
  return $ Node burrow' cost



-- ========================================================



safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x


(!?) :: V a -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) (V.toList xs) n