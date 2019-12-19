import Data.List (findIndex)
import Data.Tree
import Data.Char (ord)

main = interact $
    show . solveA . (map $ tupp . words) . lines

tupp:: [a] -> (a,a)
tupp (x:y:xs) = (x,y)



solveA :: [(String,String)] -> Int
solveA = sum . map (uncurry (*)) . zip [0..] . map length . levels . makeTree

solveB :: [(String,String)] -> Int
solveB xs = undefined

makeTree :: [(String,String)] -> Tree String
makeTree xs = unfoldTree getChildren "COM"
  where 
    getChildren parent = (parent, map snd $ filter (\x -> (fst x) == parent) xs)
