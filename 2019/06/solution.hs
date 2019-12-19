import Data.List (findIndex)
import Data.Tree
import Data.Char (ord)

main = interact $
    show . solveB . (map $ tupp . words) . lines

tupp:: [a] -> (a,a)
tupp (x:y:xs) = (x,y)


solveA :: [(String,String)] -> Int
solveA = sum . map (uncurry (*)) . zip [0..] . map length . levels . makeTree

solveB :: [(String,String)] -> Int
solveB xs = (d1 - a) + (d2 - a) - 2
  where 
    a = ancestorDepth tree "YOU" "SAN"
    d1 = findDepthWhere (\t -> (rootLabel t) == "YOU") tree
    d2 = findDepthWhere (\t -> (rootLabel t) == "SAN") tree
    tree = makeTree xs 

makeTree :: [(String,String)] -> Tree String
makeTree xs = unfoldTree getChildren "COM"
  where 
    getChildren parent = (parent, map snd $ filter (\x -> (fst x) == parent) xs)

ancestorDepth :: Eq a => Tree a -> a -> a -> Int
ancestorDepth t x y = findDepthWhere lowestAncestor t
  where
    lowestAncestor t = (hasBoth t) && (all (not . hasBoth) $ subForest t)
    hasBoth t = (t `contains` x) && (t `contains` y)

findDepthWhere :: (Tree a -> Bool) -> Tree a -> Int
findDepthWhere f t  | f t == True = 0
                    | length recurses > 0 = 1 + (minimum $ recurses)
                    | otherwise = -1
  where
    recurses = filter (>=0) $ map (findDepthWhere f) $ subForest t

contains :: Eq a =>  Tree a -> a -> Bool
contains t x  | rootLabel t == x = True
              | otherwise = any (\st -> contains st x) $ subForest t
