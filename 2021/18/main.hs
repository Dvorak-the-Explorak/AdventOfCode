{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}

import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
import Text.Parsec (Parsec(..), putState, getState, modifyState)

import Data.List (tails)
import Data.Foldable (foldl')
import Control.Monad (when)
-- import Helpers (chain)

import Debug.Trace
ttrace x = trace (show x) x
-- example
type PuzzleInput = [SnailNumber]


part1 = True

main = do
  vals <- getPuzzleInput
  -- mapM print vals
  -- putStr "\n\n\n"

  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  putStr "Part 2: "
  let result1 = solve2 vals
  print result1



getPuzzleInput :: IO PuzzleInput
getPuzzleInput = do
  input <- getContents
  let parseResult = runParser puzzle 0 "(unknown)" input
  case parseResult of
    (Left err) -> fail $ show err
    (Right puzzleInput) -> return puzzleInput


solve1 :: PuzzleInput-> Int
solve1 (x:xs) = magnitude $ foldl' addSnails x xs

solve2 :: PuzzleInput -> Int
solve2 nums = result
  where
    pairs = [(x,y) | x <- nums, y <- nums, x /= y]
    result = maximum $ map (magnitude . uncurry addSnails) pairs


data SnailNumber = Number Int Int | Pair Int (SnailNumber, SnailNumber)
  deriving Eq

instance Show SnailNumber where
  show (Number depth val) = show val
  show (Pair depth (x,y)) 
    | depth >= 4 = "{"++ show x ++ "," ++ show y ++ "}"
    | otherwise = "["++ show x ++ "," ++ show y ++ "]"

showDepth :: SnailNumber -> String
showDepth (Number depth val) = show val
showDepth (Pair depth (x,y)) 
  | depth >= 4 = show depth ++ "_{"++ showDepth x ++ "," ++ showDepth y ++ "}"
  | otherwise = show depth ++ "_["++ showDepth x ++ "," ++ showDepth y ++ "]"


type E a = a -> a


-- -- continuation with left and right continues
-- type TreeCont a = State (E SnailNumber, E SnailNumber) a


-- lCont = gets fst
-- rCont = gets snd

-- explode :: SnailNumber -> TreeCont SnailNumber
-- explode x@(Number _) = return x
-- explode (Pair (x,y)) = do
--   return 

magnitude :: SnailNumber -> Int
magnitude (Number _ n) = n
magnitude (Pair _ (x,y)) = 3 * magnitude x + 2 * magnitude y

addSnails :: SnailNumber -> SnailNumber -> SnailNumber
addSnails x y = result 
  where
  result = if depth x /= 0 || depth y /= 0
    then error $ "Depths not zero:\n" ++ showDepth x ++ "\n" ++ showDepth y
    else settle $ Pair 0 (incDepth x, incDepth y) 

settle :: E SnailNumber
settle x = result
  where
    x' = explodeAll x
    (chop, x'') = split x'
    result = if chop 
      then settle x''
      else x''


onRightmost :: (E Int) -> SnailNumber -> SnailNumber
onRightmost f (Number d n) = (Number d (f n))
onRightmost f (Pair d (l,r)) = (Pair d (l, onRightmost f r))

onLeftmost :: (E Int) -> SnailNumber -> SnailNumber
onLeftmost f (Number d n) = (Number d (f n))
onLeftmost f (Pair d (l,r)) = (Pair d (onLeftmost f l, r))





explode1 :: E SnailNumber
explode1 x = result
  where
    (boomed, result, left, right) = explode x

explodeAll :: E SnailNumber
explodeAll x = result
  where
    (boomed, x', _, _) = explode x
    result = if not boomed
              then x'
              else explodeAll x'


-- take a SnailNumber, explode its leftmost one that needs exploding
-- returns the exploded number, and a continuation of what to do to
--    the left and right neighbours
explode :: SnailNumber -> (Bool, SnailNumber, E Int, E Int)
explode x@(Number _ n) = (False, x, id, id) -- We don't explode individual numbers
explode (Pair 4 (Number _ x, Number _ y)) = (True, Number 3 0, (+x), (+y))
explode (Pair n (x, y)) = (boom, Pair n (x', y'), left, right)
  where
    (xboom, expX, lx, rx) = explode x
    (yboom, expY, ly, ry) = explode y

    (boom, x', y', left, right) = 
      if xboom  -- only boom y if x didn't boom
        then (True, expX, onLeftmost rx y, lx, id)
        -- else (yboom, onRightmost ly x, expY, id, ry)
        else if yboom 
          then (True, onRightmost ly x, expY, id, ry)
          else (False, x, y, id, id)

split1 :: E SnailNumber
split1 x = result
  where
    (chop, result) = split x


split :: SnailNumber -> (Bool, SnailNumber)
split x@(Number d n) = 
  if n>=10 
    then (True, Pair (d+1) (Number (d+1) $ n`div`2, Number (d+1) $ (n+1)`div`2) )
    else (False, x)
split (Pair d (x,y)) = (chop, Pair d (x', y'))
  where
    ( chopX, splitX) = split x
    ( chopY, splitY) = split y

    (chop, x', y') = 
      if chopX 
      then (True, splitX, y)
      else if chopY
        then (True, x, splitY)
        else (False, x, y)




depth :: SnailNumber -> Int
depth (Number d _) = d
depth (Pair d _) = d


overDepth :: (E Int) -> SnailNumber -> SnailNumber
overDepth f (Number d n) = Number (f d) n
overDepth f (Pair d (x, y)) = Pair (f d) $ (overDepth f x, overDepth f y)

incDepth :: SnailNumber -> SnailNumber
incDepth (Number d n) = Number (d+1) n
incDepth (Pair d (x, y)) = Pair (d+1) $ (incDepth x, incDepth y)













-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parsec String Int PuzzleInput
puzzle = many1 $ snailNumber <* end

snailNumber = do
  putState 0

  char '['
  x <- element
  char ','
  y <- element
  char ']'

  return $ Pair 0 (x,y)


element = try number <|> try pair <?> "Element of a SnailNumber pair"

number = do
  depth <- getState
  val <- integer
  return $ Number depth val

pair = do
  modifyState (+1)
  depth <- getState
  -- when (depth >= 4) $ fail "Invalid Snailfish number, depth cannot be >=4"

  char '['
  x <- element
  char ','
  y <- element
  char ']'

  modifyState (subtract 1)

  return $ Pair depth (x, y)






-- commonly used

integer :: Parsec String Int Int
integer = do
  negative <- optionMaybe $ char '-'
  absValue <- read <$> many1 digit
  case negative of 
    Nothing -> return absValue
    Just _ -> return $ -absValue

end :: Parsec String Int ()
end = (endOfLine >> return ()) <|> eof