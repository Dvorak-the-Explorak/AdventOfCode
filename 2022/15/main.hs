{-# LANGUAGE TupleSections #-}

import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl')
import Data.Maybe (catMaybes)
-- import Helpers (chain)


-- example
type PuzzleInput = ([Sensor], Int)
type Sensor = (Coord, Coord)
type Coord = (Int,Int)

type Interval = (Int, Int)
newtype Intervals = Intervals { unIntervals :: [Interval] }
instance Semigroup Intervals where
  (Intervals (i:is)) <> other = (Intervals is) <> addInterval i other
instance Monoid Intervals where
  mempty = Intervals []

fromList :: [Interval] -> Intervals
fromList = foldl' (flip addInterval) mempty

-- insertion sort & merge
addInterval :: Interval -> Intervals -> Intervals
addInterval (s,e) (Intervals []) = Intervals [(s,e)]
addInterval (s,e) (Intervals ((a,b):is))
  | e < a = Intervals $ (s,e):(a,b):is
  | s > b = addInterval (a,b) $ addInterval (s,e) $ Intervals is
  | otherwise = addInterval (min s a, max e b) $  Intervals is

measureIntervals :: Intervals -> Int
measureIntervals = sum . map (uncurry subtract) . unIntervals

freeSpots :: Interval -> Intervals -> [Int]
freeSpots (lo,hi) (Intervals []) = [lo..hi]
freeSpots (lo,hi) (Intervals ((a,b):is)) 
  | hi < a = []
  | otherwise = [lo..a-1] ++ freeSpots (b+1,hi) (Intervals is)

part1 = True

main = do
  vals <- getPuzzleInput

  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  putStr "Part 2: "
  let result2 = solve2 vals
  print result2

solve1 :: PuzzleInput -> Int
solve1 (sensors, query) = measureIntervals intervals
  where
    intervals = fromList $ catMaybes $ map (getInterval query) sensors

solve2 :: PuzzleInput -> Int
solve2 (sensors,_) = calcFrequency $ head $ possibleCoords
  where
    calcFrequency (x,y) = 4000000*x + y

    possibleCoords = concat [map (,y) (findXCoords y) | y <- [0..4000000]]

    findXCoords y = freeSpots (0,4000000) $ fromList $ catMaybes $ map (getInterval y) sensors

-- Get the interval that the sensor covers at the query row
getInterval :: Int -> Sensor -> Maybe Interval
getInterval query ((sx,sy), (bx,by)) = result
  where
    result = if excess > 0 
              then Just (mid - excess, mid + excess)
              else Nothing
    mid = sx
    dist = abs (bx - sx) + abs (by - sy)
    excess = dist - abs (sy - query)

getPuzzleInput :: IO PuzzleInput
getPuzzleInput = do
  input <- getContents
  let parseResult = parse puzzle "(unknown)" input
  case parseResult of
    (Left err) -> fail $ show err
    (Right puzzleInput) -> return puzzleInput

-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = do
  sensors <- many1 sensor
  query <- consult
  return (sensors, query)

--example
sensor :: Parser Sensor
sensor = do
  string "Sensor at x="
  sensor_x <- integer
  string ", y="
  sensor_y <- integer
  string ": closest beacon is at x="
  closest_x <- integer
  string ", y="
  closest_y <- integer 
  end
  return ((sensor_x,sensor_y), (closest_x,closest_y))

consult :: Parser Int
consult = do
  string "Consult y="
  integer <* end


-- commonly used

integer :: Parser Int
integer = do
  negative <- optionMaybe $ char '-'
  absValue <- read <$> many1 digit
  case negative of 
    Nothing -> return absValue
    Just _ -> return $ -absValue

    
end :: Parser ()
end = (endOfLine >> return ()) <|> eof