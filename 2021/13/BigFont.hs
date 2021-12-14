module BigFont (bigFontPoints, bigShow) where

import Data.List (foldl')
import Data.Char

import qualified Data.HashSet as Set
type Set = Set.HashSet

type BigString = [String]

on = '█'
off = '░'

bigFontPoints :: String -> Set (Int,Int)
bigFontPoints = makeSet . bigShow

bigShow :: String -> BigString
bigShow input = foldl' bigConcat bigEmpty bigLetters
  where
    bigLetters =map (pattern . up) input
    up x | isLetter x = toUpper x
         | otherwise = x

bigEmpty :: BigString
bigEmpty = [[], [], [], [], [], []]

bigConcat :: BigString -> BigString -> BigString
bigConcat = zipWith (\ a b -> a ++ [off] ++ b)

makeSet :: [String] -> Set (Int,Int)
makeSet rows = Set.fromList coords
  where
    coords = map snd $ filter ((==on) . fst) $ zip (concat rows) $ [(row,col) | row <- [0..5], col <- [0..3]]

pattern :: Char -> [String]
pattern 'A' =  ["░███░",
                "█░░░█",
                "█░░░█",
                "█████",
                "█░░░█",
                "█░░░█"]

pattern 'B' =  ["████░",
                "█░░░█",
                "████░",
                "█░░░█",
                "█░░░█",
                "████░"]

pattern 'C' =  ["░███░",
                "█░░░█",
                "█░░░░",
                "█░░░░",
                "█░░░█",
                "░███░"]

pattern 'D' =  ["████░",
                "█░░░█",
                "█░░░█",
                "█░░░█",
                "█░░░█",
                "████░"]

pattern 'E' =  ["█████",
                "█░░░░",
                "████░",
                "█░░░░",
                "█░░░░",
                "█████"]

pattern 'F' =  ["█████",
                "█░░░░",
                "████░",
                "█░░░░",
                "█░░░░",
                "█░░░░"]

pattern 'G' =  ["░███░",
                "█░░░█",
                "█░░░░",
                "█░░██",
                "█░░░█",
                "░███░"]

pattern 'H' =  ["█░░░█",
                "█░░░█",
                "█████",
                "█░░░█",
                "█░░░█",
                "█░░░█"]

pattern 'I' =  ["█████",
                "░░█░░",
                "░░█░░",
                "░░█░░",
                "░░█░░",
                "█████"]

pattern 'J' =  ["░░███",
                "░░░░█",
                "░░░░█",
                "░░░░█",
                "█░░░█",
                "░███░"]

pattern 'K' =  ["█░░░█",
                "█░░█░",
                "███░░",
                "█░█░░",
                "█░░█░",
                "█░░░█"]

pattern 'L' =  ["█░░░░",
                "█░░░░",
                "█░░░░",
                "█░░░░",
                "█░░░░",
                "█████"]

pattern 'M' =  ["█░░░█",
                "██░██",
                "█░█░█",
                "█░█░█",
                "█░░░█",
                "█░░░█"]

pattern 'N' =  ["█░░░█",
                "██░░█",
                "███░█",
                "█░███",
                "█░░██",
                "█░░░█"]

pattern 'O' =  ["░███░",
                "█░░░█",
                "█░░░█",
                "█░░░█",
                "█░░░█",
                "░███░"]

pattern 'P' =  ["████░",
                "█░░░█",
                "█░░░█",
                "████",
                "█░░░░",
                "█░░░░"]

pattern 'Q' =  ["░███░",
                "█░░░█",
                "█░░░█",
                "█░░░█",
                "█░░██",
                "░████"]

pattern 'R' =  ["████░",
                "█░░░█",
                "█░░░█",
                "████",
                "█░░█░",
                "█░░░█"]

pattern 'S' =  ["░████",
                "█░░░░",
                "░█░░░",
                "░░██░",
                "░░░░█",
                "████░"]

pattern 'T' =  ["█████",
                "░░█░░",
                "░░█░░",
                "░░█░░",
                "░░█░░",
                "░░█░░"]

pattern 'U' =  ["█░░░█",
                "█░░░█",
                "█░░░█",
                "█░░░█",
                "█░░░█",
                "░███░"]

pattern 'V' =  ["█░░░█",
                "█░░░█",
                "██░██",
                "░█░█░",
                "░███░",
                "░░█░░"]

pattern 'W' =  ["█░░░█",
                "█░░░█",
                "█░█░█",
                "█████",
                "██░██",
                "█░░░█"]

pattern 'X' =  ["█░░░█",
                "█░░░█",
                "░█░█░",
                "░░█░░",
                "░█░█░",
                "█░░░█"]

pattern 'Y' =  ["█░░░█",
                "░█░█░",
                "░░█░░",
                "░░█░░",
                "░░█░░",
                "░░█░░"]

pattern 'Z' =  ["█████",
                "░░░█░",
                "░░█░░",
                "░░█░░",
                "░█░░░",
                "█████"]

pattern ' ' =  ["░░░░░",
                "░░░░░",
                "░░░░░",
                "░░░░░",
                "░░░░░",
                "░░░░░"]

pattern _ =  ["█████",
              "█████",
              "█████",
              "█████",
              "█████",
              "█████"]
