import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl', sort)
-- import Helpers (chain)


-- example
type PuzzleInput = [Present]
type Present = [Int]


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
solve1 presents = sum $ map area presents

-- example
solve2 :: PuzzleInput -> Int
solve2 presents = sum $ map ribbon presents


area :: Present -> Int
area dims = minimum sideAreas + 2 * (sum sideAreas)
  where
    sideAreas = zipWith (*) dims $ tail dims ++ [head dims]
  
ribbon :: Present -> Int
ribbon dims = 2 * sum (take 2 $ sort dims) + product dims




-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = many1 present

--example
present :: Parser Present
present = do
  w <- integer
  char 'x'
  l <- integer
  char 'x'
  h <- integer
  end
  return [w, l, h]




-- commonly used

integer :: Parser Int
integer = read <$> many1 digit

end :: Parser ()
end = (endOfLine >> return ()) <|> eof