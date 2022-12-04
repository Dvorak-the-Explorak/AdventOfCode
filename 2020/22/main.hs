import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl', reverse)
import qualified Data.Set as Set
-- import Helpers (chain)

import Debug.Trace (trace)

-- example
type PuzzleInput = (Deck, Deck)
type Deck = [Int]
type Context = Set.Set PuzzleInput

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
solve1 ([], xs) = evaluate xs
solve1 (xs, []) = evaluate xs
solve1 ((x:xs), (y:ys)) = 
  if x > y
    then solve1 (xs ++ [x, y], ys)
    else solve1 (xs, ys ++ [y, x])


solve2 :: PuzzleInput -> Int
solve2 decks = if oneWins
          then evaluate xs
          else evaluate ys
  where
    (oneWins, (xs,ys)) = crabs Set.empty decks

crabs :: Context -> PuzzleInput -> (Bool, PuzzleInput)
crabs prev ([], ys) = (False, ([], ys))
crabs prev (xs, []) = (True, (xs, []))
crabs prev (xss@(x:xs),yss@(y:ys)) 
  | Set.member (xss,yss) prev = (True, (xss, yss))
  | otherwise = if x <= length xs && y <= length ys
                  then let (oneWins, _) = crabs (Set.insert (xss, yss) prev) (take x xs, take y ys)
                        in if oneWins 
                            then crabs (Set.insert (xss, yss) prev) (xs ++ [x, y], ys)
                            else crabs (Set.insert (xss, yss) prev) (xs, ys ++ [y, x])
                  else if x > y
                        then crabs (Set.insert (xss, yss) prev) (xs ++ [x, y], ys)
                        else crabs (Set.insert (xss, yss) prev) (xs, ys ++ [y, x])

evaluate :: Deck -> Int
evaluate = sum . map (uncurry (*)) . zip [1..] . reverse


-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = do
  x <- row
  y <- row
  return (x,y)

--example
row :: Parser Deck
row = do
  string "Player "
  integer
  char ':'
  endOfLine
  many1 (integer <* endOfLine) <* end




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