import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl')
-- import Helpers (chain)
import Debug.Trace
ttrace x = trace (show x) x

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

  putStr "Part 2: "
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

solve2 :: PuzzleInput -> Int
solve2 region@((xmin,xmax), (ymin,ymax)) = length $ filter hits velocities
  where
    hits = (==Hit) . getResult
    results = map getResult velocities
    getResult = runUntilResult (getHitOrMiss region) . start 
    start vel = ((0,0), vel)
    velocities = [(x,y) | x <- [0..xmax], y <- [ymin..(1-ymin)]]

getHitOrMiss :: PuzzleInput -> Phase -> Maybe Result
getHitOrMiss (xrange, yrange) ((x,y), (u,v)) = result
  where
    result = if inRange x xrange && inRange y yrange
          then Just Hit
          else if y < fst yrange && v < 0
            then Just Miss
            else Nothing

inRange :: Int -> Range -> Bool
inRange x (low,high) = low<=x && x<=high


step :: Phase -> Phase
step ((x,y), (u,v)) = ((x',y'), (u',v'))
  where
    x' = x + u
    y' = y + v
    u' = u - signum u
    v' = v - 1

runUntilResult :: (Phase -> Maybe Result) -> Phase -> Result
runUntilResult get phase = 
  case get phase of
    Nothing -> runUntilResult get $ step phase
    Just result -> result



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