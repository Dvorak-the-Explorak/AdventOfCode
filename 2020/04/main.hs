import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
import Debug.Trace

import Data.List (foldl')
import Data.Char (isAlphaNum, isDigit)
-- import Helpers (chain)

import Data.Map (Map, fromList, member, (!))

-- example
type PuzzleInput = [Row]
type Row = Map String String

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
solve1 = length . filter valid

solve2 :: PuzzleInput -> Int
solve2 = length . filter valid2


required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]--, "cid"]

valid :: Row -> Bool
valid pass = all (flip member pass) required

valid2 :: Row -> Bool
valid2 pass = all id [valid pass
                  ,validByr byr
                  ,validIyr iyr
                  ,validEyr eyr
                  ,validHgt hgt
                  ,validHcl hcl
                  ,validEcl ecl
                  ,validPid pid
                  ,validCid cid]
  where
    byr = pass ! "byr"
    iyr = pass ! "iyr"
    eyr = pass ! "eyr"
    hgt = pass ! "hgt"
    hcl = pass ! "hcl"
    ecl = pass ! "ecl"
    pid = pass ! "pid"
    cid = pass ! "cid"

validByr byr = all id [length byr == 4, read byr >= 1920, read byr <= 2002]
validIyr iyr = all id [length iyr == 4, read iyr >= 2010, read iyr <= 2020]
validEyr eyr = all id [length eyr == 4, read eyr >= 2020, read eyr <= 2030]
validHgt hgt = if units == "cm"
                then read num >= 150 && read num <= 193
                else read num >= 59 && read num <= 76
  where
    num = takeWhile (`elem` "0123456789") hgt
    units = drop (length num) hgt
validHcl hcl = length hcl == 7 && all isAlphaNum (tail hcl)
validEcl ecl = elem ecl ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validPid pid = length pid == 9 && all isDigit pid
validCid = const True

-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = many1 (row <* many endOfLine)

--example
row :: Parser Row
row = do
  kvs <- many1 anyKeyval
  return $ fromList kvs

anyKeyval :: Parser (String, String)
anyKeyval = do
  key <- many1 alphaNum
  char ':'
  val <- many1 (alphaNum <|> char '#')
  char ' ' <|> (end >> return ' ')
  -- return $ trace (show (key, val)) (key, val)
  return $ (key, val)

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