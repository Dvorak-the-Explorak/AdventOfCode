import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl', transpose)
import Data.Maybe (catMaybes)
import Control.State
-- import Helpers (chain)

import Debug.Trace (trace)


-- example
type PuzzleInput = ([Stack], [Instruction])
type Stack = [Char]
type Instruction = (Count, Source, Dest)
type Count = Int
type Source = Int
type Dest = Int

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


solve1 :: PuzzleInput -> [Char]
solve1 (stacks, []) = map head stacks
solve1 (stacks, (i:is)) = solve1 (move i stacks, is)

solve2 :: PuzzleInput -> [Char]
solve2 (stacks, []) = map head stacks
solve2 (stacks, (i:is)) = solve2 (moveN i stacks, is)


move :: Instruction -> [Stack] -> [Stack]
move _ [] = []
move (0,_,_) stacks = stacks
move (n,s,d) stacks = move (n-1, s, d) $ push c d stacks'
  where (c, stacks') = pop s stacks

pop :: Int -> [Stack] -> (Char, [Stack])
pop n stacks = (head $ stacks !! n, take n stacks ++ [tail $ stacks !! n] ++ drop (n+1) stacks)


push :: Char -> Int -> [Stack] -> [Stack]
push c n stacks = take n stacks ++ [c:(stacks !! n)] ++ drop (n+1) stacks


moveN :: Instruction -> [Stack] -> [Stack]
moveN _ [] = []
moveN (0,_,_) stacks = stacks
moveN (n,s,d) stacks = pushN cs d stacks'
  where (cs, stacks') = popN n s stacks

popN :: Int -> Int -> [Stack] -> ([Char], [Stack])
popN num n stacks = (take num $ stacks !! n, take n stacks ++ [drop num $ stacks !! n] ++ drop (n+1) stacks)

pushN :: [Char] -> Int -> [Stack] -> [Stack]
pushN cs n stacks = take n stacks ++ [cs ++ (stacks !! n)] ++ drop (n+1) stacks


-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = do
  rows <- many1 row
  let stacks = map (catMaybes) $ transpose rows
  instructions <- many1 instruction
  end
  return (stacks, instructions)
  

instruction :: Parser Instruction
instruction = do
  string "move "
  count <- integer
  string " from "
  source <- (subtract 1) <$> integer
  string " to "
  dest <- (subtract 1) <$> integer
  end
  return (count, source, dest)


--example
row :: Parser [Maybe Char]
row = 
  let 
    noCrate = string "   " >> return Nothing
    crate = Just <$> (char '[' >> anyChar <* char ']')
  in sepBy (crate <|> noCrate) (char ' ') <* end





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