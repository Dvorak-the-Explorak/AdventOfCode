import Control.Monad.State

main = interact $
  show . solve . tripleSums . (map read) . lines


solve :: [Int] -> Int
solve xs = snd $ execState (solve1 xs) (Nothing, 0)


solve1 :: [Int] -> State (Maybe Int, Int) ()
solve1 [] = return ()
solve1 (x:xs) = process x >> solve1 xs 

process :: Int -> State (Maybe Int, Int) ()
process x = do
  (prev,count) <- get
  case prev of
    Nothing -> put (Just x,count)
    (Just y) -> if x > y 
                  then put (Just x, count+1)
                  else put (Just x, count)





tripleSums :: [Int] -> [Int]
tripleSums = map sum . triples

triples :: [a] -> [[a]]
triples = takeWhile ((==3) . length) . tripleGen
  where
    tripleGen [] = []
    tripleGen xs = take 3 xs : tripleGen (tail xs)