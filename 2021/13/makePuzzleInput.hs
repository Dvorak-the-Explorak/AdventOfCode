import qualified Data.HashSet as Set
import System.Random

import Debug.Trace

import BigFont



type Set = Set.HashSet
type Coord = (Int,Int)
type Points = Set Coord


data Fold = Vertical Int | Horizontal Int
instance Show Fold where
  show (Vertical x) = "x=" ++ show x
  show (Horizontal y) = "y=" ++ show y

main = do
  input <- getContents
  -- mapM putStrLn $ bigShow input


  let points = bigFontPoints input

  (fold, points') <- unfold points

  printPoints points'
  putStrLn ""
  print fold

unfold :: Points -> IO (Fold, Points)
unfold points = do 
  direction <- getStdRandom random :: IO Int
  
  let fold = Vertical 3
  let points' = points


  return (fold, points')


printPoints :: Points -> IO ()
printPoints points = do
  mapM putStrLn $ map (\(r,c) -> (show r ++ "," ++ show c)) $ Set.toList points
  return ()

