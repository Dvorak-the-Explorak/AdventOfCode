import Data.List (foldl')
-- import Helpers (chain)
import Debug.Trace


ttrace x = trace (show x) x

-- example
type PuzzleInput = Game
type Player = (Int, Int)
type Game = (Player, Player)


part1 = True

main = do
  -- vals <- getPuzzleInput
  let game = ((10,0), (9,0))

  putStr "Part 1: "
  let result1 = solve1 game
  print result1

  putStr "Part 2: "
  let result1 = solve2 game
  print result1






solve1 :: Game -> Int
solve1 game = loserScore * numMoves
  where
    loserScore = min (snd $ fst $ game') (snd $ snd $ game')
    (game', numMoves) = runToFinish dice game
    dice = cycle [1..]

solve2 :: PuzzleInput -> Int
solve2 = const (-1)

runToFinish :: [Int] -> Game -> (Game, Int)
runToFinish dice game
  | finished game = (game, head dice - 1)
  | otherwise = runToFinish (drop 3 dice) $ trace (show game' ++ " " ++ show (take 3 dice) ) $ game'
      where 
        game' = step game $ sum (take 3 dice)


finished :: Game -> Bool
finished (p1, p2) = (snd p1 >= 1000) || (snd p2 >= 1000)




step :: Game -> Int -> Game
step ((p1, score1), p2) move = (p2, (p1', score1'))
  where
    p1' = ((p1 + move - 1) `mod` 10) + 1
    score1' = score1 + p1'



-- stepState :: Int -> State (Int,Int) ()
-- stepState move = do
--   (p1, p2) <- get
--   let p1' = ((p1 + move - 1) `mod` 10) + 1
--   put (p2, p1')


deinterlace :: [a] -> ([a],[a])
deinterlace [] = undefined
deinterlace [x] = undefined
deinterlace xs = unzip $ go xs
  where
    go (x:y:xs) = (x,y) : (go xs)

tripleSums :: [Int] -> [Int]
tripleSums [] = []
tripleSums [x] = [x]
tripleSums [x,y] = [x+y]
tripleSums xs = (sum (take 3 xs) :) $ tripleSums $ drop 3 xs