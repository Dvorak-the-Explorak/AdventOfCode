import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
import Data.List (transpose, foldl')

data Square = Unmarked Int | Marked Int
type Board = [[Square]]

instance Show Square where
  show (Unmarked n) = " " ++ show n
  show (Marked n) = "_" ++ show n

main = do
  input <- getContents
  let result = parse game "(unknown)" input
  case result of
    (Left err) -> print err
    (Right (calls,boards)) -> do 
      let (winner,lastCall) = firstWinner boards (0:calls)
      putStrLn "First winner:"
      print winner
      putStrLn "On call of:"
      print lastCall
      putStrLn "With score:"
      print $ lastCall * (sumUnmarked winner)

      let (winner,lastCall) = lastWinner boards (0:calls)
      putStrLn ""
      putStrLn "Last winner:"
      print winner
      putStrLn "On call of:"
      print lastCall
      putStrLn "With score:"
      print $ lastCall * (sumUnmarked winner)



firstWinner :: [Board] -> [Int] -> (Board,Int)
firstWinner boards calls  | (not . null) $ filter finished boards = (head $ filter finished boards, head calls)
                          | otherwise = firstWinner (map (mark $ head $ tail calls) boards) (tail calls)

lastWinner :: [Board] -> [Int] -> (Board,Int)
lastWinner [board] (call:calls) | finished board = (board,call)
                                | otherwise = lastWinner [(mark $ head calls) board] calls
lastWinner boards calls = lastWinner (filter (not . finished) $ map (mark $ head $ tail calls) boards) (tail calls)

isMarked :: Square -> Bool
isMarked (Marked _) = True
isMarked _ = False

mark :: Int -> Board -> Board
mark n b = map (map markSquare) b
  where
    markSquare (Unmarked m) | m==n = Marked n
    markSquare x = x

finished :: Board -> Bool
finished b = any (all isMarked) b || (any (all isMarked) $ transpose b)

sumUnmarked :: Board -> Int
sumUnmarked b = sum $ map (foldl' addUnmarked 0) b
  where 
    addUnmarked n (Marked _) = n
    addUnmarked n (Unmarked m) = n+m


-- ===============================================
--                    Parsers
-- ===============================================

callOrder :: Parser [Int]
callOrder = integer `sepBy1` (char ',') <* endOfLine


boardRow = many (char ' ') >> square `sepBy1` (many1 $ char ' ') <* endOfLine

board = many1 boardRow <* endOfLine

game = do 
  calls <- callOrder
  endOfLine
  boards <- many1 board
  eof
  return (calls, boards)

integer = read <$> many1 digit
square = Unmarked <$> integer
