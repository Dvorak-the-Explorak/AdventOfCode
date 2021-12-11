
import qualified Data.HashMap.Strict as Map

main = interact $ 
  show . solve . lines

solve :: [String] -> Int
solve input = result
  where
    result = sum $ map (corruptScore "") input

corruptScore :: String -> String -> Int
corruptScore (c:cs) (x:xs)
    | x == c = corruptScore cs xs
corruptScore cs (x:xs) 
    | x `elem` "({[<" = corruptScore (getClose x : cs) xs
    | otherwise       = closeValue x
corruptScore _ [] = 0

closeValue ')' = 3
closeValue ']' = 57
closeValue '}' = 1197
closeValue '>' = 25137
closeValue _ = 0


closeMap :: Map.HashMap Char Char
closeMap = Map.fromList [('(', ')'),
                      ('[', ']'),
                      ('{', '}'),
                      ('<', '>')]
getClose :: Char -> Char
getClose open = fromMaybe $ Map.lookup open closeMap

fromMaybe (Just x) = x