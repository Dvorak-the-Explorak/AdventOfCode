import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl', sort)
import Control.Monad (guard)
import Control.Applicative (liftA2)

-- example
type Result = Int

part1 = True

main = do

  input <- getContents

  putStr "Part 1: "
  result1 <- getParseResult input solve1
  print result1

  putStr "Part 2: "
  result2 <- getParseResult input solve2
  print result2


getParseResult :: String -> Parser a -> IO a
getParseResult input p = do
  let parseResult = parse p "(unknown)" input
  case parseResult of
    (Left err) -> fail $ show err
    (Right value) -> return value

-- =========================================================
--                             Parsers
-- =========================================================

solve1 :: Parser Result
solve1 = return (-1)

solve2 :: Parser Result
solve2 = return (-1)


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