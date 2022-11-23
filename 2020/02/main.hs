import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl')
-- import Helpers (chain)
import Debug.Trace


-- example
type PuzzleInput = [Row]
data Row = Row {
  rowMin :: Int,
  rowMax :: Int,
  rowTarget :: Char,
  rowPassword :: [Char]
} deriving Show


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


solve1 :: PuzzleInput-> Int
solve1 = length . filter valid1

solve2 :: PuzzleInput -> Int
solve2 = length . filter valid2


valid1 :: Row -> Bool
valid1 (r@(Row _min _max _target _password)) =
  let
    l = length $ filter (== _target) _password
  in
    -- trace ((show l) ++ (show r)) $ (l >= _min) && (l <= _max)
    (l >= _min) && (l <= _max)

valid2 :: Row -> Bool
valid2 (r@(Row _min _max _target _password)) =
  (==1) $ length $ filter (==_target) $ map ( \n -> _password !! (n-1)) [_min, _max]

-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = many1 row

--example
row :: Parser Row
row = do
  _min <- integer
  char '-'
  _max <- integer
  char ' '
  _target <- anyChar
  char ':'
  char ' '
  _password <- many1 alphaNum <* end
  return $ Row _min _max _target _password




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