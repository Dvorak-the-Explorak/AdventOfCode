import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
import Data.List (transpose, foldl')
import Data.List.Extra (sumOn')

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


-- Second argument includes the last call that was already made (janky hack)
firstWinner :: [Board] -> [Int] -> (Board,Int)
firstWinner boards (lastCall:calls) | any finished boards = (head $ filter finished boards, lastCall)
                                    | otherwise = firstWinner (map (mark $ head calls) boards) calls


-- Second argument includes the last call that was already made (janky hack)
lastWinner :: [Board] -> [Int] -> (Board,Int)
lastWinner [board] (lastCall:calls) | finished board = (board,lastCall)
                                    | otherwise = lastWinner [(mark $ head calls) board] calls
lastWinner boards (_:calls) = lastWinner (filter unfinished $ map (mark $ head calls) boards) calls

isMarked (Marked _) = True
isMarked _ = False

mark :: Int -> Board -> Board
mark n b = map (map markSquare) b
  where
    markSquare (Unmarked m) | m==n = Marked n
    markSquare x = x

finished :: Board -> Bool
finished b = any (all isMarked) b || (any (all isMarked) $ transpose b)

unfinished :: Board -> Bool
unfinished = not . finished

value :: Square -> Int
value (Marked _) = 0
value (Unmarked n) = n

sumUnmarked :: Board -> Int
sumUnmarked = sum . map (sumOn' value)


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
