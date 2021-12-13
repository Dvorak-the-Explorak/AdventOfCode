{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
-- import Data.Tuple.Extra (first, second)
import Data.List (foldl')

import qualified Data.HashSet as Set
import Data.Hashable

import Control.Lens (Lens, Lens', lens, view, over)
import Control.Lens.Tuple (_1, _2)


import BBox
import Utils

import Debug.Trace
ttrace x = trace (show x) x

newtype Point = Point (Int,Int)
  deriving (Eq)
instance Hashable Point where
  hashWithSalt salt (Point p) = hashWithSalt salt p
instance Show Point where
  show (Point p) = show p


getPoint (Point p) = p

_p :: Lens' Point (Int,Int)
_p = lens getPoint (const Point)

-- first f (Point (x,y)) = Point (f x, y)
-- second f (Point (x,y)) = Point (x, f y)
first = over (_p._1)
second = over (_p._2)


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

      let result = solve2 points folds
      printGrid result

solve1 :: Set Point -> [Fold] -> Int
solve1 points folds = result
  where
    result = length $ Set.toList $ doFold (head folds) points

solve2 :: Set Point -> [Fold] -> Set Point
solve2 points folds = folded
  where
    -- folded = foldl' (\ps f -> doFold f ps) points folds
    folded = doFolds folds points



printGrid :: Set Point -> IO ()
printGrid points = do
    let grid = getGrid points
    mapM putStrLn $ map concat grid
    return ()

getGrid :: Set Point -> [[String]]
getGrid points = map (map paintCoord) gridCoords
  where 
    (minx, maxx, miny, maxy) = getBBox points
    gridCoords = [[Point (x,y) | x <- [minx..maxx]] | y <- [miny..maxy]]
    
    paintCoord p = if p `Set.member` points 
                    then "#" 
                    else "."


doFolds :: [Fold] -> Set Point -> Set Point
-- doFolds points folds = foldl' (\ps f -> doFold f ps) points folds
-- doFolds = flip $ foldl' $ flip doFold
doFolds folds = chain $ (map doFold) folds

doFold :: Fold -> Set Point -> Set Point
doFold (Vertical x) = foldOn (_p._1) x
doFold (Horizontal y) = foldOn (_p._2) y

foldOn :: Lens' Point Int -> Int -> Set Point -> Set Point
foldOn l pos points = Set.union low high'
  where
    low = Set.filter ((<pos) . view l) points
    high = Set.filter ((>pos) . view l) points

    high' = Set.map (over l $ \val -> 2*pos - val) high

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