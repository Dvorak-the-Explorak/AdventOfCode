{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import qualified Data.HashSet as Set
import Data.Hashable

import Control.Lens (Lens, Lens', lens, view, over)
import Control.Lens.Tuple (_1, _2)

import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import BBox



-- sugar
type Set = Set.HashSet







-- point
newtype Point = Point (Int,Int)
  deriving (Eq)
instance Hashable Point where
  hashWithSalt salt (Point p) = hashWithSalt salt p
instance Show Point where
  show (Point p) = show p



-- fold
data Fold = Vertical Int | Horizontal Int
instance Show Fold where
  show (Vertical x) = "x=" ++ show x
  show (Horizontal y) = "y=" ++ show y




-- bounding box instances
instance BoxBound Point where
  bbox (Point p) = bbox p
instance BoxBound (Set Point) where
  bbox points = bbox $ Set.toList points










-- =========================================================
--                             Lenses
-- =========================================================

getPoint (Point p) = p

_p :: Lens' Point (Int,Int)
_p = lens getPoint (const Point)

first = over (_p._1)
second = over (_p._2)



-- =========================================================
--                             Parsers
-- =========================================================

getPuzzleInput :: IO (Set Point, [Fold])
getPuzzleInput = do
  input <- getContents
  let result = parse puzzleInput "(unknown)" input
  case result of
    (Left err) -> fail $ show err
    (Right (points, folds)) -> return (points, folds)

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