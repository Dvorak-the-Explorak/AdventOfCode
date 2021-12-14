import qualified Data.HashSet as Set
import System.Random

import Control.Lens (Lens, Lens', lens, view, over)
import Control.Lens.Tuple (_1, _2)

import Control.Monad (replicateM)

import Debug.Trace

import Types
import Functions
import BigFont

type Coord = (Int,Int)
type Points = Set Coord

main = do
  input <- getContents

  -- mapM putStrLn $ bigShow input

  let points = bigFontPoints input
  -- print points

  (folds, points') <- unfoldMany 15 points

  -- printGrid $ Set.map Point points'

  printPuzzle folds points'

printPuzzle :: [Fold] -> Points -> IO ()
printPuzzle folds points = do
  printPoints points
  putStrLn ""
  mapM (putStrLn . ("fold along " ++) . show) $ folds
  return ()

unfold :: Points -> IO (Fold, Points)
unfold points = do 
  direction <- getStdRandom random :: IO Double

  if direction < 0.5 
    then unfoldVertical points
    else unfoldHorizontal points



unfoldVertical :: Points -> IO (Fold,Points)
unfoldVertical points = do
  rands <- replicateM (length points) (getStdRandom random) :: IO [Double]

  let pos = (1+) $ maximum $ map fst $ Set.toList points

  let fold = Vertical pos

  let flippedPoint (x,y) = (2*pos - x, y)

  let unfoldPoint (p,rand) = 
        if rand < 1/4 
          then [p]
          else if rand < 1/2
            then [flippedPoint p]
            else [p, flippedPoint p]
  let points' = Set.fromList $ concatMap unfoldPoint $ zip (Set.toList points) rands 

  return (fold, points')

unfoldHorizontal :: Points -> IO (Fold,Points)
unfoldHorizontal points = do
  rands <- replicateM (length points) (getStdRandom random) :: IO [Double]

  let pos = (1+) $ maximum $ map snd $ Set.toList points

  let fold = Horizontal pos

  let flippedPoint (x,y) = (x, 2*pos - y)

  let unfoldPoint (p,rand) = 
        if rand < 1/4
          then [p]
          else if rand < 1/2
            then [flippedPoint p]
            else [p, flippedPoint p]
  let points' = Set.fromList $ concatMap unfoldPoint $ zip (Set.toList points) rands 

  return (fold, points')

unfoldMany :: Int -> Points -> IO ([Fold], Points)
unfoldMany 0 points = return ([], points)
unfoldMany n points = do
  (folds, points') <- unfoldMany (n-1) points
  (fold, points'') <- unfold points'
  return (fold:folds, points'')



printPoints :: Points -> IO ()
printPoints points = do
  mapM putStrLn $ map (\(r,c) -> (show r ++ "," ++ show c)) $ Set.toList points
  return ()

