import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
import qualified Data.HashMap.Strict as Map

import Debug.Trace

main = do
  input <- getContents
  let result = parse allLines "(unknown)" input
  case result of
    (Left err) -> print err
    (Right scores) -> print $ sum scores

line :: Parser Int
line = chunks <* restOfLine

allLines = (eof >> return []) <|> do
  score <- line 
  scores <- allLines
  return (score:scores)

closeMap :: Map.HashMap Char Char
closeMap = Map.fromList [('(', ')'),
                      ('[', ']'),
                      ('{', '}'),
                      ('<', '>')]
getClose :: Char -> Char
getClose open = fromMaybe $ Map.lookup open closeMap

fromMaybe (Just x) = x

-- restOfLine = manyTill anyChar endOfLine
restOfLine = many (noneOf "\n") >> endOfLine


chunks = do
  score <- chunk <|> return (-1)
  case score of
    -1 -> return 0 -- no chunk
    0 -> chunks -- found a valid chunk, keep going
    x -> return x


chunk = do
  open <- oneOf "({[<"
  let close = (char (getClose open) >> return 0)
                  <|> corrupt
  score <- chunks
  case score of
    (-1) -> close
    0 -> close
    x -> return x


corrupt :: Parser Int
corrupt = (char ')' >> return 3) <|> 
          (char ']' >> return 57) <|>
          (char '}' >> return 1197) <|>
          (char '>' >> return 25137) <|>
          (lookAhead endOfLine >> return 0) <|>
          (lookAhead eof >> return 0) <?> "Corruption"


ttrace x = trace (show x) x