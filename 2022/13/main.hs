import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl', intercalate)
-- import Helpers (chain)


-- example
type PuzzleInput = [Pair]
type Pair = (Packet, Packet)
data Packet = Number Int | Packet [Packet]
instance Show Packet where
  show (Number n) = show n
  show (Packet []) = "[]"
  show (Packet xs) = "[" ++ (intercalate "," $ map show xs) ++ "]"
instance Eq Packet where
  p1 == p2 = compare p1 p2 == EQ
instance Ord Packet where
  compare (Number n1) (Number n2) = compare n1 n2
  compare (Packet []) (Packet []) = EQ
  compare (Packet []) (Packet _) = LT
  compare (Packet _) (Packet []) = GT
  compare (Packet (x:xs)) (Packet (y:ys)) = 
    case compare x y of
      EQ -> compare (Packet xs) (Packet ys)
      o -> o
  compare (Number n) (Packet p) = compare (Packet [Number n]) (Packet p)
  compare (Packet p) (Number n) = compare (Packet p) (Packet [Number n])

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
solve1 vals = sum $ map snd $ filter fst $ zip correct [1..]
  where
    correct = map (uncurry (<)) vals
-- solve1 = sum . map fst . filter snd . zip [1..] . (map $ uncurry (<))

-- Don't bother sorting, just count number less than each marker
solve2 :: PuzzleInput -> Int
solve2 vals = (1 + length smaller1) * (2 + length smaller2) 
  where
    marker1 = Packet [Packet [Number 2]]
    marker2 = Packet [Packet [Number 6]]

    smaller1 = filter (< marker1) allVals
    smaller2 = filter (< marker2) allVals

    allVals = concat $ map (\(x,y) -> [x,y]) vals

-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = many1 (pair <* end)

--example
pair :: Parser Pair
pair = do
  p1 <- packet
  endOfLine
  p2 <- packet
  end
  return (p1, p2)


packet :: Parser Packet
packet = (Number <$> integer) <|> do
  char '['
  vals <- sepBy packet $ char ','
  char ']'
  return $ Packet vals

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