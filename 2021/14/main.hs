import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import qualified Data.HashMap.Strict as Map
import Data.Hashable

import Data.List (foldl')
import Data.List.Extra (maximumOn, minimumOn)
-- import Helpers (chain)

import Debug.Trace
ttrace x = trace (show x) x

type Map = Map.HashMap 


-- example
type PuzzleInput = (String, Rules)
type Rules = Map String Char
type Rule = (String,Char)


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


-- example
solve1 :: PuzzleInput-> Int
solve1 (template, rules) = most - least
  where
    subbed = (iterate (substitute rules) template) !! 10
    counts = ttrace $ Map.elems $ getCounts subbed
    most = maximum counts
    least = minimum counts

-- example
solve2 :: PuzzleInput -> Int
solve2  (template, rules) = most - least
  where
    subbed = (iterate (substitute rules) template) !! 40
    counts = ttrace $ Map.elems $ getCounts subbed
    most = maximum counts
    least = minimum counts



substitute :: Rules -> String -> String
substitute rules (x:y:xs) = 
  case Map.lookup [x,y] rules of
    Nothing -> (x:) $ substitute rules (y:xs)
    Just c -> ([x,c] ++ ) $ substitute rules (y:xs)
substitute rules xs = xs


getCounts :: (Eq a, Hashable a) => [a] -> Map.HashMap a Int
getCounts = foldl' (\ acc x -> tally x acc) $ Map.fromList []
  where
    tally x = Map.insertWith (+) x 1



-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = do
  template <- manyTill anyChar end
  end
  rules <- many1 rule
  return (template, Map.fromList rules)

--example
rule :: Parser Rule
rule = do
  input <- many1 letter
  string " -> "
  output <- letter
  end
  return (input, output)




-- commonly used

integer :: Parser Int
integer = read <$> many1 digit

end :: Parser ()
end = (endOfLine >> return ()) <|> eof

