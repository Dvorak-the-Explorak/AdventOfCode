import Data.List (foldl', partition)
import qualified Data.HashMap.Strict as Map
import Helpers 


import Debug.Trace


type Map = Map.HashMap








-- example
type PuzzleInput = Game
type Player = (Int, Int)
type Game = (Player, Player)

type GameState = (Bool, Int, Int, Int, Int)




part1 = True

main = do
  -- vals <- getPuzzleInput
  let game1 = ((10,0), (9,0))

  putStr "Part 1: "
  let result1 = solve1 game1
  print result1

  putStr "Part 2: "
  let game2 =  [((True, 10, 0, 9, 0), 1)]
  -- let game2 =  [((True, 4, 0, 8, 0), 1)]
  let result1 = solve2 game2
  print result1



diceCounts = getCountsList $ map sum [[i,j,k] | i <- [1,2,3], j <- [1,2,3], k<-[1,2,3]]




solve1 :: Game -> Int
solve1 game = loserScore * numMoves
  where
    loserScore = min (snd $ fst $ game') (snd $ snd $ game')
    (game', numMoves) = runToFinish dice game
    dice = cycle [1..]



runToFinish :: [Int] -> Game -> (Game, Int)
runToFinish dice game
  | finished game = (game, head dice - 1)
  | otherwise = runToFinish (drop 3 dice) $ game'
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









-- =============================================================================================
-- =============================================================================================
-- =============================================================================================






solve2 :: [(GameState, Int)] -> Int
solve2 game 
  | all (gameFinished . fst) game = getAnswer game
  | otherwise = solve2 $ stepDirac game


getAnswer :: [(GameState, Int)] -> Int
getAnswer counts =  max p1Wins p2Wins
  where
    p1Wins = sum $ map snd $ filter (player1Wins . fst) counts
    p2Wins = sum $ map snd $ filter (not . player1Wins . fst) counts



player1Wins :: GameState -> Bool
player1Wins (p1turn, p1, s1, p2, s2) = s1 >= 21



gameFinished :: GameState -> Bool
gameFinished (p1turn, p1, s1, p2, s2) = s1 >= 21 || s2 >= 21


stepDirac :: [(GameState, Int)] -> [(GameState, Int)]
stepDirac stateCounts = Map.toList result
  where
    result = foldl' (Map.unionWith (+)) (Map.fromListWith (+) done) nextCounts

    (done, going) = partition (gameFinished . fst) stateCounts

    -- get the map of next states, multiply by current count
    nextCount (g, count) = Map.map (*count) $ diracUpdate g
    nextCounts = map nextCount going
    


diracUpdate :: GameState -> Map GameState Int
diracUpdate g = Map.fromListWith (+) $ zip newStates counts
  where
    newStates = map (stepGame g) rolls
    rolls = [3..9]
    counts = [1, 3, 6, 7, 6, 3, 1]


stepGame :: GameState -> Int -> GameState 
stepGame g@(p1turn, p1, s1, p2, s2) roll = result
  where 
    p1' = ((p1 + roll - 1) `mod` 10) + 1    
    p2' = ((p2 + roll - 1) `mod` 10) + 1    



    result = if gameFinished g
      then g
      else if p1turn 
        then (not p1turn, p1', s1+p1',   p2,  s2)
        else (not p1turn, p1,  s1,       p2', s2+p2')













