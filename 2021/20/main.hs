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

-- solve1 :: PuzzleInput -> Int
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

decodeImage :: Encoding -> Grid Char -> Grid Char
decodeImage enc grid = mapKernel (decodeKernel enc) $ pad '0' grid


naiveStep :: Int -> Encoding -> Grid Char -> Grid Char
naiveStep 0 enc g = g
naiveStep n enc g = naiveStep (n-1) enc $ decodeImage enc g


-- reverses the encoding every step, as each step the board silently flips
toggleStep :: Int -> Encoding -> Grid Char -> Grid Char
toggleStep 0 enc g = g
toggleStep n enc g = toggleStep (n-1) (V.reverse $ flipEncoding enc) $ decodeImage enc g


step :: Int -> Encoding -> Grid Char -> Grid Char
step 0 enc g = g
step n enc g = case (V.head enc, V.last enc) of
  -- sea always 0, no modifications
  ('0', _) -> naiveStep n enc g
  -- sea alternates between 0 and 1
  ('1', '0') -> (if n `mod` 2 == 1 then flipGrid else id)  $ toggleStep (n-1) (V.reverse enc) $ decodeImage (flipEncoding enc) g
  -- sea stays at 1.  
  ('1', '1') -> undefined -- not sure if the line below is accurate
  -- ('1', '1') -> flipGrid $ naiveStep (n-1) (V.reverse $ flipEncoding enc) $ decodeImage (flipEncoding enc) g


flipEncoding :: Vec Char -> Vec Char
flipEncoding = V.map flipPixel

flipGrid = fmap flipPixel

flipPixel '0' = '1'
flipPixel '1' = '0'

pad :: a -> Grid a -> Grid a
pad val grid@(Grid vals) = (Grid $ [newRow] ++ vals' ++ [newRow])
  where
    newRow = take (n+2) $ repeat val
    vals' = map (\row -> [val] ++ row ++ [val]) vals
    (_,n) = gridDim grid

-- assume grid is padded with 1 layer of the "infinite sea" value
decodeKernel :: Encoding -> Kernel Char Char
decodeKernel enc = \getVal coord -> (enc !) $ indexKernel getVal coord

indexKernel :: Kernel Char Int
indexKernel = \getVal coord -> binToInt $ map (maybe '0' id . getVal) $ squareKernel coord

binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc*2 + digitToInt x) 0

