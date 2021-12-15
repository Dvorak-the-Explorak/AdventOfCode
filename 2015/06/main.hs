import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl')
import qualified Data.HashMap.Strict as Map

import Helpers (chain)

type Map = Map.HashMap


-- example
type PuzzleInput = [Instruction]
data Instruction = Toggle Coord Coord | On Coord Coord | Off Coord Coord
type Coord = (Int,Int)


part1 = True

main = do
  vals <- getPuzzleInput

  -- putStr "Part 1: "
  -- let result1 = solve1 vals
  -- print result1

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


-- #TODO Instead of keeping track of every individual light, 
--  make a data structure to intersect the squares

solve1 :: PuzzleInput-> Int
solve1 instructions = length $ filter (==True) $ Map.elems finalLights
  where
    finalLights :: Map Coord Bool
    finalLights = chain (map follow instructions) lights

    follow (On tl br) = mapOnSquare1 on1 tl br 
    follow (Off tl br) = mapOnSquare1 off1 tl br 
    follow (Toggle tl br) = mapOnSquare1 toggle1 tl br 

    -- coords = [(x,y) | x <- [0..999], y <- [0..999]]
    lights = Map.empty

solve2 :: PuzzleInput -> Int
solve2 instructions = sum $ Map.elems finalLights
  where
    finalLights :: Map Coord Int
    finalLights = chain (map follow instructions) lights

    follow (On tl br) = mapOnSquare2 on2 tl br 
    follow (Off tl br) = mapOnSquare2 off2 tl br 
    follow (Toggle tl br) = mapOnSquare2 toggle2 tl br 

    -- coords = [(x,y) | x <- [0..999], y <- [0..999]]
    lights = Map.empty

mapOnSquare1 :: (Maybe Bool -> Maybe Bool) -> Coord -> Coord ->  Map Coord Bool -> Map Coord Bool
mapOnSquare1 f tl br lights = chain (map go1 coords) lights
  where
    -- go1 :: Coord -> Map Coord Bool -> Map Coord Bool
    go1 coord = Map.alter f coord 
    coords = [(x,y) | x <- [minx..maxx], y <- [miny..maxy]]
    inSquare (x,y) = x>=minx && x<=maxx && y>=miny && y<=maxy
    (minx, miny) = tl
    (maxx, maxy) = br

mapOnSquare2 :: (Maybe Int -> Maybe Int) -> Coord -> Coord ->  Map Coord Int -> Map Coord Int
mapOnSquare2 f tl br lights = chain (map go1 coords) lights
  where
    -- go1 :: Coord -> Map Coord Bool -> Map Coord Bool
    go1 coord = Map.alter f coord 
    coords = [(x,y) | x <- [minx..maxx], y <- [miny..maxy]]
    inSquare (x,y) = x>=minx && x<=maxx && y>=miny && y<=maxy
    (minx, miny) = tl
    (maxx, maxy) = br






toggle1 :: Maybe Bool -> Maybe Bool
toggle1 Nothing = Just True
toggle1 (Just True) = Nothing
toggle1 (Just False) = Just True

on1 :: Maybe Bool -> Maybe Bool
on1 _ = Just True

off1 :: Maybe Bool -> Maybe Bool
off1 _ = Nothing




toggle2 :: Maybe Int -> Maybe Int
toggle2 Nothing = Just 2
toggle2 (Just n) = Just (n+2)

on2 :: Maybe Int -> Maybe Int
on2 Nothing = Just 1
on2 (Just n) = Just (n+1)

off2 :: Maybe Int -> Maybe Int
off2 (Just n) 
  | n<2 = Nothing
  | otherwise = Just (n-1)
off2 Nothing = Nothing



-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = many1 instruction

instruction = do
  action <- (try turnOn) <|> (try turnOff) <|> (try toggle)
  (x1,y1) <- coord
  string " through "
  (x2,y2) <- coord
  end

  let topLeft = (min x1 x2, min y1 y2)
  let botRight = (max x1 x2, max y1 y2)

  return $ action topLeft botRight


turnOn = string "turn on " >> return On
turnOff = string "turn off " >> return Off
toggle = string "toggle " >> return Toggle

coord = do
  x <- integer
  char ','
  y <- integer
  return (x,y)

-- commonly used

integer :: Parser Int
integer = read <$> many1 digit

end :: Parser ()
end = (endOfLine >> return ()) <|> eof