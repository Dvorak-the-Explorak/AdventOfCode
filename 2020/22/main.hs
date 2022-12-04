import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl', reverse)
import Data.Either.Extra (fromEither, isLeft)
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
solve1 decks = solve1 $ nextHand (>) decks

solve2 :: PuzzleInput -> Int
solve2 = evaluate . fromEither . crabs Set.empty



nextHand :: (Int -> Int -> Bool) -> PuzzleInput -> PuzzleInput
nextHand p ((x:xs), (y:ys)) =
  if p x y
    then (xs ++ [x, y], ys)
    else (xs, ys ++ [y, x])

crabs :: Context -> PuzzleInput -> Either Deck Deck
crabs prev ([], ys) = Right ys
crabs prev (xs, []) = Left xs
crabs prev decks@(x:xs, y:ys)
  | Set.member decks prev = Left $ fst decks
  | otherwise = let 
                  context = Set.insert decks prev
                  subGameResult = isLeft $ crabs context (take x xs, take y ys)
                in if shouldRecurse decks
                    then crabs context $ nextHand (const $ const subGameResult) decks
                    else crabs context $ nextHand (>) decks

shouldRecurse :: PuzzleInput -> Bool
shouldRecurse ([], _) = False
shouldRecurse (_, []) = False
shouldRecurse (x:xs, y:ys) = hasAtLeastN x xs && hasAtLeastN y ys

hasAtLeastN :: Int -> [a] -> Bool
hasAtLeastN n xs = length (take n xs) == n

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