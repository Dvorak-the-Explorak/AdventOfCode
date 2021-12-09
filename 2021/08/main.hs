import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
import Data.List (foldl', permutations, sort, elemIndex, delete)
import Data.Char (chr, ord)
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)

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
          let outputs = map solveSegment2 segments
          print $ sum outputs

countPred p xs = foldl' (\ acc x -> if p x then acc+1 else acc) 0 xs

solveSegment2 :: ([String], [String]) -> Int
solveSegment2 (raw_inputs, outputs) = result
  where
    inputs = map sort raw_inputs
    one = head $ filter (lengthIs 2) inputs
    four = head $ filter (lengthIs 4) inputs 
    seven = head $ filter (lengthIs 3) inputs 
    eight = head $ filter (lengthIs 7) inputs 

    letterCounts = getCounts $ concat inputs
    b_enc = fst $ head $ Map.toList $ Map.filter (==6) letterCounts
    e_enc = fst $ head $ Map.toList $ Map.filter (==4) letterCounts
    f_enc = fst $ head $ Map.toList $ Map.filter (==9) letterCounts
    c_enc = head $ delete f_enc one


    -- remove the solved ones
    inputs' = foldl' (\acc x -> delete x acc) inputs [one, four, seven, eight]

    zero = head $ filter (\x -> all (flip elem x) [b_enc,e_enc,f_enc,c_enc]) inputs'
    six = head $ filter (\x -> not (elem c_enc x) && all (flip elem x) [b_enc,e_enc,f_enc]) inputs'
    two = head $ filter (elem e_enc &&& (not . elem f_enc) &&& (not . elem b_enc)) inputs'
    three = head $ filter (elem f_enc &&& (not . elem e_enc) &&& (not . elem b_enc)) inputs'
    five = head $ filter (lengthIs 5 &&& elem f_enc &&& (not . elem e_enc) &&& elem b_enc) inputs'
    nine = head $ filter (lengthIs 6 &&& elem f_enc &&& (not . elem e_enc) &&& elem b_enc) inputs'

    lengthIs n = (==n) . length

    valMap = Map.fromList $ [(zero,0),(one,1),(two,2),(three,3),(four,4),(five,5),(six,6),(seven,7),(eight,8),(nine,9)]

    getJust Nothing = error $ "Uh OH"
    getJust (Just x) = x

    result = digitsToInt $ map (getJust . \x -> Map.lookup x valMap) $ map sort outputs






(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) p q x = (p x) && (q x)



getCounts :: (Eq a, Hashable a) => [a] -> Map.HashMap a Int
getCounts = foldl' (\ acc x -> tally x acc) $ Map.fromList []
  where
    tally x = Map.insertWith (+) x 1


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