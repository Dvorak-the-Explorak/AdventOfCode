import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl')
import Control.Monad.State
-- import Helpers (chain)
import Debug.Trace (trace)
ttrace x = trace (show x) x

-- example
type PuzzleInput = [Op]
data Op = Noop | Addx Int


part1 = True

main = do
  vals <- getPuzzleInput

  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  putStrLn "Part 2: "
  let result2 = solve2 vals
  mapM putStrLn result2



getPuzzleInput :: IO PuzzleInput
getPuzzleInput = do
  input <- getContents
  let parseResult = parse puzzle "(unknown)" input
  case parseResult of
    (Left err) -> fail $ show err
    (Right puzzleInput) -> return puzzleInput


solve1 :: PuzzleInput-> Int
solve1 ops = sum $ everyN 40 $ (drop 19) strength
  where
    strength = zipWith (*) (runOps ops 1) [1..]

solve2 :: PuzzleInput -> [String]
solve2 ops = chunkN 40 $ map drawPixel screen
  where
    vals = runOps (cycle ops) 1
    indices = [0..239]
    screen = trace debug2 $ zipWith near vals indices
    debug1 = unlines $ map show $ chunkN 40 $ zip vals indices
    debug2 = unlines $ map show $ chunkN 40 $ zip vals $ map horiz indices

-- cycle :: [a] -> [a]
-- cycle xs = xs ++ cycle xs

chunkN :: Int -> [a] -> [[a]]
chunkN _ [] = []
chunkN n xs = take n xs : chunkN n (drop n xs)

drawPixel :: Bool -> Char
drawPixel True = '#'
drawPixel False = '.'


everyN :: Int -> [a] -> [a]
everyN _ [] = []
everyN n xs = head xs : everyN n (drop n xs)

runOp :: Op -> [Int] -> [Int]
runOp Noop xs = [last xs]
runOp (Addx x) xs = [last xs, x + last xs]

runOps :: [Op] -> Int -> [Int]
runOps ops n = concat $ scanl (flip runOp) [n] ops

near :: Int -> Int -> Bool
near x y  | horiz x == horiz (y-1) = True
          | horiz x == horiz y = True
          | horiz x == horiz (y+1) = True
          | otherwise = False

horiz :: Int -> Int
-- horiz x = ((x-1) `mod` 40) + 1
horiz = (`mod` 40)

-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = many1 row

--example
row :: Parser Op
row = (try noop <|> try addx) <* end

noop :: Parser Op
noop = do
  string "noop"
  return Noop

addx :: Parser Op
addx = do
  string "addx "
  Addx <$> integer


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