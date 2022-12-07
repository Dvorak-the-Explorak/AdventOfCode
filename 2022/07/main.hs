import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl', sort)
import Data.Maybe (catMaybes) 
-- import Helpers (chain)
import Control.Monad (guard)


import Debug.Trace (trace)

-- example
type PuzzleInput = Directory
data Directory = Dir String Int [Directory] | File String Int

instance Show Directory where
  show (File name size) = "f" ++ show (name, size)
  show (Dir name size dirs) = "d(" ++ show name ++ "," ++ show size ++ "," ++ show (map dirName dirs) ++ ")" 

dirSize :: Directory -> Int
dirSize (File _ x) = x
dirSize (Dir _ s _) = s

dirName :: Directory -> String
dirName (File n _) = n
dirName (Dir n _ _) = n

isDir :: Directory -> Bool
isDir (File _ _) = False
isDir (Dir _ _ _) = True

allDirs :: Directory -> [Directory]
allDirs (File _ _) = []
allDirs d@(Dir _ _ ds) = d:(concat $ map allDirs ds)

part1 = True

main = do
  root <- getPuzzleInput
  -- mapM putStrLn $ map show $ allDirs root

  putStr "Part 1: "
  let result1 = solve1 root
  print result1

  putStr "Part 2: "
  let result2 = solve2 root
  print result2



getPuzzleInput :: IO PuzzleInput
getPuzzleInput = do
  input <- getContents
  let parseResult = parse puzzle "(unknown)" input
  case parseResult of
    (Left err) -> fail $ show err
    (Right puzzleInput) -> return puzzleInput


solve1 :: PuzzleInput-> Int
solve1 = sum . filter (<= 100000) . map dirSize . filter isDir . allDirs 
-- solve1 d = trace (show d) 1
  -- where
  --   showDir (Dir name size _) = (name,size)
  --   dirs = show $ map showDir $ allDirs d

solve2 :: PuzzleInput -> Int
solve2 root = head $ filter (>= required) $ sort $ map dirSize $ filter isDir $ allDirs root
  where
    required = dirSize root - (70000000 - 30000000)






-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = cd

command = (try ls <|> try ((:[]) <$> cd))

cd :: Parser Directory
cd = do
  string "$ cd "
  name <- manyTill anyChar endOfLine
  guard $ name /= ".."
  -- contents :: [Directory]
  contents <- concat <$> many1 command <* end_cd
  let size = sum $ map dirSize contents
  return (Dir name size contents)

end_cd :: Parser String
end_cd = (string "$ cd .." <* endOfLine) <|> (eof >> return "")

-- just gets files
ls :: Parser [Directory]
ls = do
  string "$ ls"
  end
  catMaybes <$> many1 ls_result

ls_result :: Parser (Maybe Directory)
ls_result = (directory >> return Nothing) <|> (Just <$> file)

directory :: Parser ()
directory = (do
 string "dir "
 manyTill anyChar end
 return ()) <?> "directory"

file :: Parser Directory
file = (do
  size <- integer
  char ' '
  name <- manyTill anyChar end
  return (File name size))  <?> "file"

-- commonly used

integer :: Parser Int
integer = (do
  negative <- optionMaybe $ char '-'
  absValue <- read <$> many1 digit
  case negative of 
    Nothing -> return absValue
    Just _ -> return $ -absValue) <?> "integer"

    
end :: Parser ()
end = (endOfLine >> return ()) <|> eof