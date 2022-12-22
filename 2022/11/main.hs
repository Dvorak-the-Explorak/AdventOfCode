import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl', sortOn)
import Control.Monad.State
import Control.Applicative (liftA2)
-- import Helpers (chain)
import Debug.Trace (trace)

ttrace x = trace (show x) x

newtype ModInt = ModInt { unModInt :: Int }

makeModInt :: Int -> ModInt
makeModInt = ModInt . (`mod` modulus)
  where modulus = 2*3*5*7*11*13*17*19
  -- where modulus = 23*19*13*17

boredom :: ModInt -> ModInt
boredom = id
-- boredom = makeModInt . (`div` 3) . unModInt

plus :: ModInt -> ModInt -> ModInt
plus (ModInt x) (ModInt y) = makeModInt $ x + y

times :: ModInt -> ModInt -> ModInt
times (ModInt x) (ModInt y) = makeModInt $ x * y

-- example
type PuzzleInput = [Monkey]
type Items = [ModInt]
type Op = ModInt -> ModInt
type Target = ModInt -> Int
data Monkey = Monkey Items Op Target Int

instance Show Monkey where
  show (Monkey _ _ _ n) = show n

monkeyEmpty :: Monkey -> Bool
monkeyEmpty (Monkey [] _ _ _) = True
monkeyEmpty _ = False

inspections :: Monkey -> Int
inspections (Monkey _ _ _ n) = n

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
solve1 = product . take 2 . sortOn negate . evalState (runRounds 20 >> gets (map inspections))

solve2 :: PuzzleInput -> Int
solve2 = product . take 2 . sortOn negate . evalState (runRounds 10000 >> gets (map inspections))

runRounds :: Int -> State [Monkey] ()
runRounds n = mapM (const runAllMonkeys) [1..n] >> return ()

runAllMonkeys :: State [Monkey] ()
runAllMonkeys = do
  count <- gets length
  mapM runMonkey [0..count-1]
  return ()
  -- monkeys <- get
  -- trace (show $ map inspections monkeys) $ return ()

runMonkey :: Int -> State [Monkey] ()
runMonkey n = do
  monk <- gets (!!n)
  if monkeyEmpty monk
    then return ()
    else stepMonkey n >> runMonkey n

stepMonkey :: Int -> State [Monkey] ()
stepMonkey n = do
  Monkey items op target inspections <- gets (!!n)
  let newVal = op $ head items
  let nextMonkey = fromIntegral $ target newVal
  sendToMonkey nextMonkey newVal
  modify $ setAt n $ Monkey (tail items) op target (inspections + 1)

sendToMonkey :: Int -> ModInt -> State [Monkey] ()
sendToMonkey n val = do
  modify $ modifyAt n $ appendItem val

appendItem :: ModInt -> Monkey -> Monkey
appendItem item (Monkey items op target inspections) = Monkey (items ++ [item]) op target inspections


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
puzzle = many1 monkey

monkey :: Parser Monkey
monkey = do 
  string "Monkey "
  integer
  char ':'
  endOfLine
  items <- _items
  op <- (boredom .) <$> _op
  target <- _target
  end
  return $ Monkey items op target 0


_items :: Parser Items
_items = do
  many1 $ char ' '
  string "Starting items: "
  worry `sepBy` (string ", ") <* endOfLine

_op :: Parser Op
_op = (try square) <|> do
  whitespace >> string "Operation: new = old "
  op <- binOp
  char ' '
  (op <$> worry) <* endOfLine


binOp :: Parser (ModInt -> ModInt -> ModInt)
binOp = (char '+' >> return plus) <|> (char '*' >> return times)

square :: Parser Op
square = do
  whitespace >> string "Operation: new = old * old"
  endOfLine
  return $ makeModInt . (^2) . unModInt

_target :: Parser Target
_target = do
  whitespace >> string "Test: divisible by "
  test <- integer
  endOfLine

  whitespace >> string "If true: throw to monkey "
  trueTarget <- integer
  endOfLine

  whitespace >> string "If false: throw to monkey "
  falseTarget <- integer
  end

  return $ \ (ModInt n) -> if n `mod` test == 0
                            then trueTarget
                            else falseTarget


worry :: Parser ModInt
worry = makeModInt <$> integer

-- commonly used

integer :: Parser Int
integer = do
  negative <- optionMaybe $ char '-'
  absValue <- read <$> many1 digit
  case negative of 
    Nothing -> return absValue
    Just _ -> return $ -absValue

whitespace :: Parser String
whitespace = many1 $ char ' '
    
end :: Parser ()
end = (endOfLine >> return ()) <|> eof
