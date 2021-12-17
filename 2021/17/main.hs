import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.Maybe
import Data.Tuple.Extra (first, second)

import Data.List (foldl')
-- import Helpers (chain)
import Debug.Trace
ttrace x = trace (show x) x
ttrace' f x = trace (show $ f x) x

-- example
type PuzzleInput = (Range, Range)
type Range = (Int, Int)

type Phase = (Position, Velocity)
type Position = (Int, Int)
type Velocity = (Int, Int)

data Result = Miss | Hit
  deriving (Eq, Show)


part1 = True

main = do
  vals <- getPuzzleInput

  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  putStr "Part 2:  "
  let result1 = solve2 vals
  print result1




getPuzzleInput :: IO PuzzleInput
getPuzzleInput = do
  input <- getContents
  let parseResult = parse puzzle "(unknown)" input
  case parseResult of
    (Left err) -> fail $ show err
    (Right puzzleInput) -> return puzzleInput


solve1 :: PuzzleInput-> Int
solve1 region@((xmin,xmax), (ymin,ymax)) = result
  where
    -- for the case that a triangular number lands in the xrange, 
    -- xmin is positive and ymin is negative
    --   the answer is just triangle number (abs ymin - 1)
    result = if any (>= xmin) $ takeWhile (<= xmax) $ map tri [1..]
      then tri (-1-ymin)
      else (-1)

    tri n = n*(n+1) `div` 2

-- assumes ymax < 0 (otherwise the upper bound for y might be harder to find)
solve2 :: PuzzleInput -> Int
solve2 region@((xmin,xmax), (ymin,ymax)) = hits
  where
    -- #TODO find the largest n st. tri n < xmin
    --  use this as lower bound (as any smaller than that will stop before the target)
    xvels = [0..xmax]
    yvels = [ymin..(1-ymin)]

    -- The list of results generated for each starting x velocity
    xhits = map xResults xvels
    -- The delay before a result was returned, and how many hits in a row after that
    yhits = filter ((/=0) . snd) $ map (hitRangeY . yResults) yvels

    hits = sum $ [(length $ filter (isHit yrange) xhits) | yrange <- yhits]

    isHit (ydelay, yhits) xrs = any (==Just Hit) $ split ydelay yhits xrs

    -- get the list of Maybe Results from then starting point
    xResults = map (resultX (xmin,xmax)) . iterate stepx . start
    yResults v = map (resultY (ymin,ymax)) $ iterate stepy $ start v

    hitRangeY = second (length . takeWhile (==Just Hit)) . delayAndHits 

    delayAndHits = first length . span (==Nothing)

    split i j = take j . drop i

    start vel = (0, vel)


stepy (y,v) = (y+v, v-1)
stepx (x,u) = (x+u, u - signum u)

resultY :: Range -> (Int,Int) -> Maybe Result
resultY (ymin,ymax) (y,v)  =
  if y < ymin 
    then Just Miss
    else if y <= ymax
      then Just Hit
      else Nothing

resultX :: Range -> (Int,Int) -> Maybe Result
resultX (xmin,xmax) (x,u) =
  if x > xmax
    then Just Miss
    else if x >= xmin
      then Just Hit
      else if u == 0
        then Just Miss
        else Nothing


cross f g (x,y) = (f x, g y)

-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = do
  string "target area: "
  xrange <- range
  string ", "
  yrange <- range
  return (xrange, yrange)

--example
range :: Parser Range
range = do
  direction <- anyChar
  char '='
  low <- integer
  string ".."
  high <- integer
  return (low, high)



-- commonly used
integer :: Parser Int
integer =  do
  negative <- optionMaybe $ char '-'
  absValue <- read <$> many1 digit
  case negative of 
    Nothing -> return absValue
    Just _ -> return $ -absValue
  

end :: Parser ()
end = (endOfLine >> return ()) <|> eof