import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl', transpose)
import Data.Maybe (catMaybes)
import Control.Monad.State
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
solve1 (stacks, instrs) = map head $ evalState(mapM move1S instrs >> get) stacks

solve2 :: PuzzleInput -> [Char]
solve2 (stacks, instrs) = map head $ evalState(mapM moveS instrs >> get) stacks


move1S :: Instruction -> State [Stack] ()
move1S (0,_,_) = return ()
move1S (n,s,d) = do
  payload <- popS 1 s
  pushS payload d
  move1S (n-1, s, d)


moveS :: Instruction -> State [Stack] ()
moveS (c,s,d) = do
  payload <- popS c s
  pushS payload d

popS :: Int -> Int -> State [Stack] [Char]
popS num source = do
  targetStack <- gets (!! source)
  let payload = take num targetStack
  modify $ setAt source $ drop num targetStack
  return payload

pushS :: [Char] -> Int -> State [Stack] ()
pushS payload dest = modify $ modifyAt dest $ (payload ++) 

setAt :: Int -> a -> [a] -> [a]
setAt _ _ [] = []
setAt 0 val (x:xs) = val:xs
setAt n val (x:xs) = (x:) $ setAt (n-1) val xs

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ [] = []
modifyAt 0 f (x:xs) = (f x):xs
modifyAt n f (x:xs) = (x:) $ modifyAt (n-1) f xs


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