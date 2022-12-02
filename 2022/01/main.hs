import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl', sort)
-- import Helpers (chain)

type PuzzleInput = [Elfbag]
type Elfbag = [Int]

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
solve1 = maximum . map sum

solve2 :: PuzzleInput -> Int
solve2 = sum . take 3 . reverse . sort . map sum


-- =========================================================
--                             Parsers
-- =========================================================

puzzle :: Parser PuzzleInput
puzzle = many1 elfbag

elfbag :: Parser Elfbag
elfbag = many1 (integer <* endOfLine) <* end

-- Option so you don't need `map sum` in both solves
-- elfbag :: Parser Elfbag
-- elfbag = sum <$> many1 (integer <* endOfLine) <* end

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
