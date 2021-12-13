{-# LANGUAGE FlexibleInstances #-}

import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
-- import Data.Tuple.Extra (first, second)
import Data.List (foldl')

import qualified Data.HashSet as Set
import Data.Hashable

import BBox

import Debug.Trace
ttrace x = trace (show x) x

newtype Point = Point (Int,Int)
  deriving (Eq)
instance Hashable Point where
  hashWithSalt salt (Point p) = hashWithSalt salt p
instance Show Point where
  show (Point p) = show p
getPoint (Point p) = p
first f (Point (x,y)) = Point (f x, y)
second f (Point (x,y)) = Point (x, f y)


data Fold = Vertical Int | Horizontal Int
instance Show Fold where
  show (Vertical x) = "x=" ++ show x
  show (Horizontal y) = "y=" ++ show y

type Set = Set.HashSet





instance BoxBound Point where
  bbox (Point p) =  boxedPoint p

instance BoxBound (Set Point) where
  bbox points = bbox $ Set.toList points



part1 = True

main = do
  input <- getContents
  let result = parse puzzleInput "(unknown)" input
  case result of
    (Left err) -> print err
    (Right (points, folds)) -> do
      putStr "Part 1: "
      print $ solve1 points folds
      
      putStrLn $ solve2 points folds


solve1 :: Set Point -> [Fold] -> Int
solve1 points folds = result
  where
    result = length $ Set.toList $ doFold (head folds) points

solve2 :: Set Point -> [Fold] -> String
solve2 points folds = result
  where
    folded = foldl' (\ps f -> doFold f ps) points folds

    (minx, maxx, miny, maxy) = getBBox folded
    gridCoords = groupsOf (maxx - minx + 1) $ [Point (x,y) | y <- [miny..maxy], x <- [minx..maxx]]
    paintCoord p = if p `Set.member` folded then "#" else "."
    result = concatMap (++"\n") $ map (concatMap paintCoord) gridCoords
    
doFold :: Fold -> Set Point -> Set Point
doFold (Vertical x) points = foldVertical x points
doFold (Horizontal y) points = foldHorizontal y points


-- fold along a horizontal line
foldHorizontal :: Int -> Set Point -> Set Point
foldHorizontal y points = Set.union up down'
  where
    up = Set.filter ((<y) . snd . getPoint) points
    down = Set.filter ((>y) . snd . getPoint) points
    -- there are no points on the line (given in puzzle)

    -- 2*y - _y == y - (_y - y)
    down' = Set.map (second $ \_y -> 2*y - _y) down


-- fold along a vertical line
foldVertical :: Int -> Set Point -> Set Point
foldVertical x points = Set.union left right'
  where
    left = Set.filter ((<x) . fst . getPoint) points
    right = Set.filter ((>x) . fst . getPoint) points
    -- there are no points on the line (given in puzzle)

    -- 2*x - _x == x - (_x - x)
    right' = Set.map (first $ \_x -> 2*x - _x) right


groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs) 













-- =========================================================
--                             Parsers
-- =========================================================

puzzleInput :: Parser (Set Point, [Fold])
puzzleInput = do
  points <- many point
  endOfLine
  folds <- many fold
  return (Set.fromList points, folds)

point :: Parser Point
point = do
  x <- integer
  char ','
  y <- integer
  endOfLine
  return $ Point (x,y)

fold :: Parser Fold
fold = do
  string "fold along "
  direction <- anyChar
  char '='
  location <- integer
  (endOfLine >> return ()) <|> eof

  if direction == 'y'
    then return $ Horizontal location -- y=const is horizontal line
    else return $ Vertical location

integer :: Parser Int
integer = read <$> many1 digit 