import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.List (foldl', sort, sortOn)
import Helpers (unique)
import Control.Monad

import Debug.Trace
ttrace x = trace (show x) x

-- example
type PuzzleInput = [Action]
type Action = (Bool, Box)
type Box = (Range, Range, Range)
type Range = (Int,Int)

data RangeArrangement = Disjoint | 
                        CrossLR | CrossRL | 
                        StackLR | StackRL |
                        OverLR | OverRL | 
                        Equal

part1 = True

main = do
  vals <- getPuzzleInput

  -- print $ split1D (0,10) (20, 30)
  -- print $ split1D (20, 30) (0,10)
  -- print $ split1D (0,20) (10, 30)
  -- print $ split1D (10, 30) (0,20)
  -- print $ split1D (0, 10) (10,20)
  -- print $ split1D (0, 30) (10,20)

  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  putStr "Part 2: "
  let result1 = solve2 vals
  print result1



getPuzzleInput :: IO PuzzleInput
getPuzzleInput = do
  input <- getContents
  let parseResult = parse puzzle "(unknown)" input
  case parseResult of
    (Left err) -> fail $ show err
    (Right puzzleInput) -> return puzzleInput


-- solve1 :: PuzzleInput-> Int
solve1 (a1:a2:as) = sum $ map sizeAction ints
  where
    result@(l, ints, r) = splitAction a1 a2

    -- result = foldl' join (head actions) (tail actions)



solve2 :: PuzzleInput -> Int
solve2 = const (-1)




-- getPairs :: [a] -> [(a,a)]
-- getPairs [] = []
-- getPairs [x] = []
-- getPairs (x:y:xs) = (x,y):(getPairs (y:xs))

getPairs [] = []
getPairs xs = zip xs (tail xs)

size :: Box -> Int
size (x,y,z) = sum $ map (uncurry subtract) [x,y,z]

sizeAction :: Action -> Int
sizeAction (_, b) = size b

splitAction :: Action -> Action -> ([Action], [Action], [Action])
splitAction (dir1, b1) (dir2, b2) = (lefts', ints', rights')
  where
    lefts' = map (tuplate dir1) lefts
    ints' = map (tuplate dir2) ints
    rights' = map (tuplate dir2) rights
    (lefts, ints, rights) = split b1 b2

    tuplate x y = (x,y)

split :: Box -> Box -> ([Box], [Box], [Box])
split (x1,y1,z1) (x2,y2,z2) = (lefts, ints, rights)
  where
    lefts = [(x,y,z) | (x,xi) <- lx, (y,yi) <- ly, (z,zi) <- lz, xi*yi*zi /= 1]
    ints = [(x,y,z) | x <- intX, y <- intY, z <- intZ]
    rights = [(x,y,z) | (x,xi) <- rx, (y,yi) <- ry, (z,zi) <- rz, xi*yi*zi /= 1]

    lx = (zip leftX $ repeat 0) ++ (zip intX $ repeat 1)
    ly = (zip leftY $ repeat 0) ++ (zip intY $ repeat 1)
    lz = (zip leftZ $ repeat 0) ++ (zip intZ $ repeat 1)

    rx = (zip rightX $ repeat 0) ++ (zip intX $ repeat 1)
    ry = (zip rightY $ repeat 0) ++ (zip intY $ repeat 1)
    rz = (zip rightZ $ repeat 0) ++ (zip intZ $ repeat 1)


    (leftX, intX, rightX) = split1D x1 x2
    (leftY, intY, rightY) = split1D y1 y2
    (leftZ, intZ, rightZ) = split1D z1 z2

split1D :: Range -> Range -> ([Range], [Range], [Range])
split1D r1@(min1, max1) r2@(min2, max2) = result
  where
    result = case intersect1D r1 r2 of
      Nothing -> ([r1], [], [r2])
      Just int -> (removeRange r1 int, [int], removeRange r2 int)
    -- result = (lefts, ints, rights)

    -- lefts = shiftEnds $ getPairs $ filter (in1 &&& out2) xs
    -- ints = getPairs $ filter (in1 &&& in2) xs
    -- rights = shiftStarts $ getPairs $ filter (in2 &&& out1) xs

    -- in1 x = (x>=min1) && (x<=max1)
    -- in2 x = (x>=min2) && (x<=max2)
    -- out1 x = (x <= min1) || (x >= max1)
    -- out2 x = (x <= min2) || (x >= max2)

    -- xs = sort [min1, max1, min2, max2]


removeRange r1@(min1, max1) r2@(min2, max2) = result
  where
    result = filter (\(x,y) -> y>=x) $ parse 0 xs
    
    xs = sortOn fst [(min1,0), (max1,0), (min2,1), (max2,1)]

    parse _ [] = []
    parse _ [x] = []
    parse 0 ((_,1):xs) = parse 1 xs
    parse 1 ((x,1):xs) = parse 0 $ (x+1,0):xs
    parse 0 ((x1,0):(x2,1):xs) = (x1,x2-1):(parse 1 xs)
    parse 1 ((x1,0):(x2,1):xs) = (parse 0 $ (x2+1,0):xs)
    parse 0 ((x1,0):(x2,0):xs) = (x1,x2):(parse 0 $ (x2+1,0):xs)
    parse 1 ((x1,0):(x2,0):xs) = (parse 1 $ xs)

shiftStarts = filter (\(x,y) -> y>=x) . map (\(x,y) -> (x+1,y))
shiftEnds = filter (\(x,y) -> y>=x) . map (\(x,y) -> (x,y-1))



disjoint :: Box -> Box -> Bool
disjoint (x1, y1, z1) (x2,y2,z2) = any (uncurry disjoint1D) $ zip [x1,y1,z1] [x2,y2,z2]

disjoint1D :: Range -> Range -> Bool
disjoint1D (min1, max1) (min2, max2) = max1 < min2 || max2 < min1

intersect1D :: Range -> Range -> Maybe Range
intersect1D r1@(min1, max1) r2@(min2, max2) = do
  when (disjoint1D r1 r2) Nothing

  return $ (max min1 min2, min max1 max2)








(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) p q x = (p x) && (q x)

-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = many1 instructionP


instructionP = do
  action <- try (string "on" >> return True) <|>
           try (string "off" >> return False)
  char ' '
  box <- boxP
  return $ (action, box)


boxP = do
  string "x="
  x <- rangeP
  string ",y="
  y <- rangeP
  string ",z="
  z <- rangeP
  end
  return (x,y,z)

rangeP = do
  low <- integer
  string ".."
  high <- integer
  return $ (low,high)
    


-- commonly used

integer :: Parser Int
integer = do
  negative <- optionMaybe $ char '-'
  absValue <- read <$> many1 digit
  case negative of 
    Nothing -> return absValue
    Just _ -> return $ -absValue

end :: Parser ()
end = (endOfLine >> return ()) <|> eof