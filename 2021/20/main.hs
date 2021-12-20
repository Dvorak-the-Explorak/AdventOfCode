import Data.List (foldl')
import Data.Char (digitToInt)
import qualified Data.Vector as V
-- import Helpers (chain)

import Grid



import Debug.Trace
ttrace x = trace (show x) x


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

-- solve1 :: PuzzleInput -> Grid a
solve1 (enc, input) = num1s
  where 
    num1s = sum $ map (length . filter (=='1')) $ getGrid grid'
    grid' = step 2 enc input

-- solve2 :: PuzzleInput -> Int
solve2 (enc, input) = num1s
  where 
    num1s = sum $ map (length . filter (=='1')) $ getGrid grid'
    grid' = step 50 enc input










charToBin :: Char -> Char
charToBin '#' = '1'
charToBin '.' = '0'

decodeImage :: Encoding -> Char -> Grid Char -> Grid Char
decodeImage enc sea grid = mapKernel (decodeKernel sea enc) $ pad sea grid

step :: Int -> Encoding -> Grid Char -> Grid Char
step 0 enc g = g
step n enc g = step (n-1) enc $ decodeImage enc sea g
-- step n enc g = step (n-1) enc $ pad sea g
  where
    sea = getIndex (0,0) g

pad :: a -> Grid a -> Grid a
pad val grid@(Grid vals) = (Grid $ [newRow] ++ vals' ++ [newRow])
  where
    newRow = take (n+2) $ repeat val
    vals' = map (\row -> [val] ++ row ++ [val]) vals
    (_,n) = gridDim grid

-- assume grid is padded with 1 layer of the "infinite sea" value
decodeKernel :: Char -> Encoding -> Kernel Char Char
decodeKernel sea enc = \getVal coord -> (enc !) $ indexKernel sea getVal coord

indexKernel :: Char ->  Kernel Char Int
indexKernel sea = \getVal coord -> binToInt $ map (maybe sea id . getVal) $ squareKernel coord

binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc*2 + digitToInt x) 0

