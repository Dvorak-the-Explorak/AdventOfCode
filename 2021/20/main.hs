import Data.List (foldl')
import Data.Char (digitToInt)
import qualified Data.Vector as V
-- import Helpers (chain)

import qualified Data.HashMap.Strict as Map

import Grid

import Debug.Trace
ttrace x = trace (show x) x

type Map = Map.HashMap
type Vec = V.Vector


(!) = (V.!)

type PuzzleInput = (Encoding, Image)
type Image = Grid Char

type Encoding = Vec Char




part1 = True

main = do
  vals <- getPuzzleInput

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
    grid' = doubleStep 2 enc input

-- solve2 :: PuzzleInput -> Int
solve2 (enc, input) = num1s
  where 
    num1s = sum $ map (length . filter (=='1')) $ getGrid grid'
    grid' = doubleStep 50 enc input



charToBin :: Char -> Char
charToBin '#' = '1'
charToBin '.' = '0'

decodeImage :: Encoding -> Grid Char -> Grid Char
decodeImage enc grid = mapKernel (decodeKernel enc) $ pad '0' grid


step :: Int -> Encoding -> Grid Char -> Grid Char
step 0 enc g = g
step n enc g = step (n-1) enc $ decodeImage enc g

doubleStep :: Int -> Encoding -> Grid Char -> Grid Char
doubleStep 0 enc g = g
doubleStep 1 enc g = step 1 enc g
doubleStep n enc g = doubleStep (n-2) enc $ mapKernel (decodeTwoStepKernel enc) $ doublePad g


doublePad :: Grid Char -> Grid Char
doublePad grid = pad '0' $ pad '0' grid


pad :: a -> Grid a -> Grid a
pad val grid@(Grid vals) = (Grid $ [newRow] ++ vals' ++ [newRow])
  where
    newRow = take (n+2) $ repeat val
    vals' = map (\row -> [val] ++ row ++ [val]) vals
    (_,n) = gridDim grid

-- assume grid is padded with 1 layer of the "infinite sea" value
decodeKernel :: Encoding -> Kernel Char Char
decodeKernel enc = \getVal coord -> (enc !) $ indexKernel getVal coord

-- assume grid is padded with 1 layer of the "infinite sea" value
decodeTwoStepKernel :: Encoding -> Kernel Char Char
decodeTwoStepKernel enc = \getVal coord -> calcTwoStep enc $ map (map (maybe '0' id . getVal)) $ fiveSquare coord

indexKernel :: Kernel Char Int
indexKernel = \getVal coord -> binToInt $ map (maybe '0' id . getVal) $ squareKernel coord

binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc*2 + digitToInt x) 0


calcTwoStep :: Encoding -> [[Char]] -> Char
calcTwoStep enc input = result
  where
    result = getIndex (2, 2) grid
    grid = mapKernel (decodeKernel enc) $ mapKernel (decodeKernel enc) $ Grid input
