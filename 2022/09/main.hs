import Data.List (foldl', scanl)
-- import Helpers (chain)
import Control.Monad.State
import qualified Data.Set as Set
import Control.Applicative (liftA2)

import Debug.Trace (trace)
ttrace x = trace (show x) x

-- example
type PuzzleInput = [Instr]
type Instr = (Char, Int)
type Loc = (Int, Int)
type Hist = Set.Set Loc




part1 = True

main = do
  vals <- getPuzzleInput

  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  putStr "Part 2: "
  let result2 = solve2 vals
  print result2

getPuzzleInput :: IO PuzzleInput
getPuzzleInput = map (\x -> (x!!0, read $ drop 2 x)) <$> lines <$> getContents 

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

step :: Instr -> State ([Loc] , Hist) ()
step (_, 0) = return ()
step (dir, n) = do
  -- Get the first knot
  (x,y) <- gets $ head . fst 
  -- Move the first knot to newLoc according to one step of the instruction
  let newLoc = case dir of 
                'U' -> (x, y+1)
                'D' -> (x, y-1)
                'L' -> (x-1, y)
                'R' -> (x+1, y)
  -- Replace the head of the list of knots
  modify $ first $ (newLoc:) . tail
  -- Run physics
  modify $ first $ cascade update
  -- Update 
  modify $ updateHistory
  step (dir, n-1)

updateHistory :: ([Loc], Hist) -> ([Loc], Hist)
updateHistory (vals, hist) = (vals, Set.insert (last vals) hist)

update :: Loc -> Loc -> Loc
update p@(px,py) c@(x,y) = if dist p c <= 1 then (x,y) else (x', y')
  where
    x' = x + signum (px - x)
    y' = y + signum (py - y)

dist :: Loc -> Loc -> Int
dist (x1,y1) (x2,y2) = max (abs $ x1 - x2) (abs $ y1 - y2)

-- cascade :: (a -> a -> a) -> [a] -> [a]
-- cascade _ [] = []
-- cascade _ [x] = [x]
-- cascade f (p:x:xs) = p:cascade f ((f p x):xs)

-- cascade :: (a -> a -> a) -> [a] -> [a]
-- cascade _ [] = []
-- cascade f (x:xs) = reverse $ foldl' acc [x] xs
--   where
--     --  call f on latest output and next value, 
--     --  add the result to list of outputs
--     -- acc prevs x = f (head prevs) x : prevs
--      --acc prevs = (:prevs) . f (head prevs)
--     -- acc = liftA2 (.) (flip (:)) (f . head)
--     acc = liftA2 fmap (flip (:)) $ f . head -- fucking lol

-- cascade :: (a -> a -> a) -> [a] -> [a]
-- cascade f [] = []
-- cascade f (x:xs) = x : zipWith f (cascade f (x:xs)) xs

cascade :: (a -> a -> a) -> [a] -> [a]
cascade f [] = []
cascade f (x:xs) = scanl f x xs


solve1 :: PuzzleInput-> Int
solve1 instrs = evalState (mapM step instrs >> gets (Set.size . snd)) initialState
  where 
    initialState = (take 2 $ repeat (0,0), Set.fromList [])


solve2 :: PuzzleInput -> Int
solve2 instrs = evalState (mapM step instrs >> gets (Set.size . snd)) initialState
  where 
    initialState = (take 10 $ repeat (0,0), Set.fromList [])
