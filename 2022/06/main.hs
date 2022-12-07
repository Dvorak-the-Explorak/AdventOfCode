import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl', sort)
import Control.Monad (guard)
import Control.Applicative (liftA2)



-- example
type PuzzleInput = Row
type Row = Int

part1 = True

main = do

  input <- getContents

  putStr "Part 1: "
  result1 <- parsePuzzle 4 input
  print result1

  putStr "Part 2: "
  result2 <- parsePuzzle 14 input
  print result2



parsePuzzle :: Int -> String -> IO Int
parsePuzzle n input = do
  let parseResult = parse (puzzle n) "(unknown)" input
  case parseResult of
    (Left err) -> fail $ show err
    (Right puzzleInput) -> return puzzleInput


unique :: (Ord a, Eq a) => [a] -> [a]
unique = trim . sort
  where
    trim (x:y:xs) | x == y = trim (x:xs)
                  | otherwise = (x:) $ trim (y:xs)
    trim xs = xs



-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Int -> Parser PuzzleInput
puzzle n = length <$> prefix (marker n)

prefix :: Parser String -> Parser String
prefix marker = try marker <|> liftA2 (:) anyChar (prefix marker)

-- prefix :: Parser String -> Parser String
-- prefix marker = try marker <|> do
--   c <- anyChar
--   (c:) <$> prefix marker

marker :: Int -> Parser String
marker n = do
  chars <- mapM (const anyChar) [1..n]
  guard $ length (unique chars) == n
  return chars



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