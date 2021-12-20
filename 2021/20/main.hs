import Data.List (foldl')
import Data.Char (digitToInt)
import qualified Data.Vector as V
-- import Helpers (chain)

import qualified Data.HashMap.Strict as Map
import Control.Monad.State

import Grid

import Debug.Trace
ttrace x = trace (show x) x

type Map = Map.HashMap
type Vec = V.Vector


(!) = (V.!)

type PuzzleInput = (Encoding, Image)
type Image = Grid Char

type Encoding = Vec Char


-- =====================================================

-- I made KernelT which returns values in a monad,
--  and a corresponding runKernelT (using a traversable 
--  instance on Grid).  That let me thread the state of 
--  the two-step-encoding map through the kernel calculations.
-- Gets the right answer for step 1 (either input), but not step 2.

-- =====================================================





part1 = True

main = do
  vals <- getPuzzleInput

  -- print $ fiveSquare (0,0)

  putStrLn "Part 1: "
  let result1 = solve1 vals
  print result1

  putStr "Part 2: "
  let result1 = solve2 vals
  print result1

getPuzzleInput :: IO PuzzleInput
getPuzzleInput = do
  (encodingString:_:inputString) <- lines <$> getContents

  let encoding = map charToBin encodingString
  let input = map (map charToBin) inputString

  return (V.fromList encoding, pad '0' $ Grid input)

-- solve1 :: PuzzleInput -> Int
solve1 (enc, input) = num1s
  where 
    num1s = sum $ map (length . filter (=='1')) $ getGrid grid'
    grid' = evalState (doubleStep 1 enc input) Map.empty

-- solve2 :: PuzzleInput -> Int
solve2 (enc, input) = num1s
  where 
    num1s = sum $ map (length . filter (=='1')) $ getGrid grid'
    grid' = evalState (doubleStep 25 enc input) Map.empty



charToBin :: Char -> Char
charToBin '#' = '1'
charToBin '.' = '0'

decodeImage :: Encoding -> Grid Char -> Grid Char
decodeImage enc grid = mapKernel (decodeKernel enc) $ pad '0' grid

decodeImageMemo :: Encoding -> Grid Char -> State EncodingMap (Grid Char)
decodeImageMemo enc grid = mapKernelT (decodeKernelMemo enc) $ pad '0' $ pad '0' grid


step :: Int -> Encoding -> Grid Char -> Grid Char
step 0 enc g = g
step n enc g = step (n-1) enc $ decodeImage enc g

doubleStep :: Int -> Encoding -> Grid Char -> State EncodingMap (Grid Char)
doubleStep 0 enc g = return g
doubleStep n enc g = do
  g' <- decodeImageMemo enc g
  doubleStep (n-1) enc g'



pad :: a -> Grid a -> Grid a
pad val grid@(Grid vals) = (Grid $ [newRow] ++ vals' ++ [newRow])
  where
    newRow = take (n+2) $ repeat val
    vals' = map (\row -> [val] ++ row ++ [val]) vals
    (_,n) = gridDim grid

-- assume grid is padded with 1 layer of the "infinite sea" value
decodeKernel :: Encoding -> Kernel Char Char
decodeKernel enc = \getVal coord -> (enc !) $ indexKernel getVal coord

decodeKernelMemo :: Encoding -> KernelT (State EncodingMap) Char Char
decodeKernelMemo enc = \getVal coord -> twoStepEncoding enc $ map (map (maybe '0' id . getVal)) $ fiveSquare coord




indexKernel :: Kernel Char Int
indexKernel = \getVal coord -> binToInt $ map (maybe '0' id . getVal) $ squareKernel coord

binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc*2 + digitToInt x) 0


type EncodingMap = Map [[Char]] Char

calcTwoStep :: Encoding -> [[Char]] -> Char
calcTwoStep enc input = result
  where
    result = getIndex (2, 2) grid
    grid = mapKernel (decodeKernel enc) $ mapKernel (decodeKernel enc) $ Grid input

twoStepEncodingNoState :: EncodingMap -> Encoding -> [[Char]] -> (EncodingMap, Char)
twoStepEncodingNoState memo enc input = 
  case Map.lookup input memo of
    Just x -> (memo, x)
    Nothing -> (memo', result)
  where
    memo' = Map.insert input result memo
    result = calcTwoStep enc input

twoStepEncoding :: Encoding -> [[Char]] -> State EncodingMap Char
twoStepEncoding enc input = do
  memo <- get

  let result = calcTwoStep enc input
  let memo' = Map.insert input result memo

  case Map.lookup input memo of
    Just x -> return x
    Nothing -> put memo' >> return result



group3 :: [a] -> [[a]]
group3 [] = []
group3 [x] = []
group3 [x,y] = []
group3 xs = (take 3 xs :) $ group3 $ drop 3 xs