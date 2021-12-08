import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
import Data.List (foldl', permutations, sort, elemIndex)
import Data.Char (chr, ord)
import qualified Data.HashMap.Strict as Map

import Debug.Trace

part1 = False

main = do
  input <- getContents
  let result = parse segments "(unknown)" input
  case result of
    (Left err) -> print err
    (Right segments) -> 
      if part1
        then print $ sum $ map (countPred (\x -> length x `elem` [2,3,4,7]) . snd) segments
        else do
          let perm = "abcdegf"

          -- print $ map (runPerm perm) outputs
          let outputs = map solveSegment segments
          print $ sum outputs

countPred p xs = foldl' (\ acc x -> if p x then acc+1 else acc) 0 xs

solveSegment :: ([String], [String]) -> Int
solveSegment (inputs, outputs) = result
  where
    perm = head $ filter (checkPerm inputs) $ permutations "abcdefg"
    fixedOutputs = map (runPerm perm) outputs

    getJust item Nothing = error $ "Uh OH" ++ show item
    getJust item (Just x) = x

    outputVals = map (\x -> getJust x (elemIndex x segmentsInNum)) fixedOutputs
    result = digitsToInt $ outputVals

digitsToInt :: [Int] -> Int
digitsToInt = foldl' (\ acc n -> acc*10 + n) 0

runPerm :: String -> String -> String
runPerm perm = sort . map _map 
  where _map x = perm !! (ord x - ord 'a')

checkPerm :: [String] -> String -> Bool
checkPerm input perm = sortedNums == (sort $ map (runPerm perm) input)

sortedNums = sort segmentsInNum

segmentsInNum :: [String]
segmentsInNum = ["abcefg",
                  "cf",
                  "acdeg",
                  "acdfg",
                  "bcdf",
                  "abdfg",
                  "abdefg",
                  "acf",
                  "abcdefg",
                  "abcdfg"]





type SSInt = String

ttrace x = trace (show x) x



-- ==============================================================
--                            Parsers        
-- ==============================================================

segments = many (segment <* ((endOfLine >> return ()) <|> eof))

segment = do
  patterns <- ssint `endBy` (char ' ') 
  string "| "
  outputs <- ssint `sepBy` (char ' ')
  return (patterns,outputs)


ssint = many1 letter