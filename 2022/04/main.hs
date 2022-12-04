import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl')
-- import Helpers (chain)


-- example
type PuzzleInput = [Row]
type Row = (Range, Range)
type Range = (Int, Int)


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
getPuzzleInput = do
  input <- getContents
  let parseResult = parse puzzle "(unknown)" input
  case parseResult of
    (Left err) -> fail $ show err
    (Right puzzleInput) -> return puzzleInput


solve1 :: PuzzleInput -> Int
solve1 = sum . map nested

solve2 :: PuzzleInput -> Int
solve2 = sum . map overlap

nested :: (Range, Range) -> Int
nested (x,y) = if inside x y || inside y x
                then 1
                else 0

overlap :: (Range, Range) -> Int
overlap ((x1,x2), (y1, y2)) = 
  if x2 >= y1 && x1 <= y2
    then 1
    else 0

inside :: Range -> Range -> Bool
inside (x1, x2) (y1, y2) = x1 >= y1 && x2 <= y2



-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = many1 row

--example
row :: Parser Row
row = do
  r1 <- range
  char ','
  r2 <- range
  end
  return (r1, r2)

range :: Parser (Int, Int)
range = do
  x1 <- integer
  char '-'
  x2 <- integer
  return (x1, x2)


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