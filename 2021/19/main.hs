import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import qualified Data.HashSet as Set

import Data.List (foldl')
-- import Helpers (chain)


type Set = Set.HashSet



type PuzzleInput = [Scanner]
type Scanner = Set Coord

data Coord = Coord {
  x :: Int,
  y :: Int,
  z :: Int
}

instance Show Coord where
  show (Coord x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

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
solve1 = const (-1)

solve2 :: PuzzleInput -> Int
solve2 = const (-1)






-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = many1 row

--example
scannerP :: Parser Scanner
scannerP = do
  -- Parse the title line
  string "--- scanner "
  scannerID <- integer
  string " ---"
  endOfLine


  points <- many1 coordP <* end
  return $ Set.fromList points

coordP :: Parser Coord
coordP = do
  x <- integer
  y <- integer
  z <- integer
  return $ Coord x y z









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