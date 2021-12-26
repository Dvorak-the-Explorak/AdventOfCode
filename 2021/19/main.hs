import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import qualified Data.HashSet as Set
import Data.Hashable

import Data.List (foldl', permutations, sort)
-- import Helpers (chain)
import Debug.Trace

ttrace x = trace (show x) x

type Set = Set.HashSet



type PuzzleInput = [Scanner]
type Scanner = Set Coord
type Rotation = ((Int,Int,Int), (Int,Int,Int))
type Offset = Coord

data Coord = Coord {
  x :: Int,
  y :: Int,
  z :: Int
} deriving (Eq)

instance Show Coord where
  show (Coord x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"
instance Hashable Coord where
  hashWithSalt s (Coord x y z) = hashWithSalt s (x,y,z)

part1 = True

main = do
  vals <- getPuzzleInput

  -- mapM print vals
  -- putStrLn ""


  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  -- putStr "Part 2: "
  -- let result2 = solve2 vals
  -- print result2



getPuzzleInput :: IO PuzzleInput
getPuzzleInput = do
  input <- getContents
  let parseResult = parse puzzle "(unknown)" input
  case parseResult of
    (Left err) -> fail $ show err
    (Right puzzleInput) -> return puzzleInput

-- solve1 :: PuzzleInput-> Int
-- solve1 scanners = Set.size $ combineAll scanners
solve1 [a,b,c,d,e] = Set.size $ combineAll [a,e]

solve2 :: PuzzleInput -> Int
solve2 = const (-1)




matchesRequired = 12


combineAll ::[Scanner] -> Scanner
combineAll [x] = x
combineAll scanners = result
  where
    scanners' = combine scanners
    result = if length scanners' == 1
              then head scanners' 
              else if length scanners == length scanners'
                    then error $ "Couln't combine scanners!"
                    else combineAll scanners'

combine :: [Scanner] -> [Scanner]
combine [] = []
combine [x] = [x]
combine (x:y:xs) = case glue x y of
                  Nothing -> (x:) $ combine (y:xs)
                  -- Nothing -> trace ("Can't combine: " ++ show (x, y) ++ "\n") $ (x:) $ combine (y:xs)
                  Just g -> trace (show g) $ combine (g:xs)

-- glue :: Scanner -> Scanner -> Maybe Scanner
-- glue s1 s2 = trace ("Offsets:\n" ++ show offsetsX ++ "\n" ++ show offsetsY ++ "\n" ++ show offsetsZ ++ "\n\n") $ Set.union s1 . (uncurry offset) <$> result
--   where

--     -- result = safeHead $ filter (\x -> if ((>=matchesRequired) $ match s1 $ x) then trace (show x) True else False) options
--     result = safeHead $ filter ((>=matchesRequired) . match s1) options

--     options = [(o,s) | o <- allOffsets,  s <- allRotations s2]
--     allOffsets = [origin] ++ [Coord x y z | x <- offsetsX, y <- offsetsY, z <- offsetsZ]

--     offsetsX = offsets1D projX s1 s2
--     offsetsY = offsets1D projY s1 s2
--     offsetsZ = offsets1D projZ s1 s2


glue :: Scanner -> Scanner -> Maybe Scanner
glue s1 s2 = Set.union s1 <$> (uncurry offset) <$> result
-- glue s1 s2 = trace (show $ (map fst) options ) $ Set.union s1 <$> (uncurry offset) <$> result
  where
    result = safeHead $ filter ((>=matchesRequired) . match s1) options

    options  :: [(Coord, Scanner)]
    options = concatMap getOffsets $ allRotations s2

    getOffsets :: Scanner -> [(Coord, Scanner)]
    getOffsets s = map (\o -> (o,s)) $ allOffsets s

    allOffsets :: Scanner -> [Coord]
    allOffsets s = [origin] ++ [Coord x y z | x <- offsetsX s, y <- offsetsY s, z <- offsetsZ s]

    offsetsX :: Scanner -> [Int]
    offsetsX s = offsets1D projX s1 s
    offsetsY s = offsets1D projY s1 s
    offsetsZ s = offsets1D projZ s1 s
    -- offsetsX :: Scanner -> [Int]
    -- offsetsX s = [-20]
    -- offsetsY s = [-1133]
    -- offsetsZ s = [1061]


-- How many points match in the overlapping regions,
--  returns 0 if any unmatched points in overlap
match :: Scanner -> (Offset, Scanner)  -> Int
match scan1 (off, scan2) = result
  where
    result = if coords1 == coords2 
              then Set.size coords1 
              else 0
    scan2' = offset off scan2
    coords2 = Set.filter (inBox off) scan1
    coords1 = Set.filter (inBox origin) scan2'
 


inBox :: Coord -> Coord -> Bool
inBox (Coord x y z) (Coord cx cy cz) = xin && yin && zin
  where
    xin = abs (cx - x) <= 1000
    yin = abs (cy - y) <= 1000
    zin = abs (cz - z) <= 1000

origin = Coord 0 0 0


offsets1D :: (Coord -> Int) -> Scanner -> Scanner -> [Int]
offsets1D proj s1 s2 = filter twelve [minOffset..maxOffset]
-- offsets1D proj s1 s2 = trace ("\n" ++ show s1' ++ "\n" ++ show (map (subtract 68) s2') ++ "\n") $ filter twelve [-2000..2000]
  where
    -- lists of projected coordinates
    s1' = sort $ map proj $ Set.toList s1
    s2' = sort $ map proj $ Set.toList s2

    maxOffset = last s1' - head s2'
    minOffset = head s1' - last s2'

    twelve off = (>=matchesRequired) $ countMatches s1' $ map (+off) s2'

    countMatches _ [] = 0
    countMatches [] _ = 0
    countMatches (x:xs) (y:ys) 
      | x == y = 1 + countMatches xs ys
      | x < y = countMatches xs (y:ys)
      | otherwise = countMatches (x:xs) ys


diff xs = zipWith subtract xs (tail xs)



-- projX :: Scanner -> [Int]
-- projX scanner = Set.map (\(Coord x _ _) -> x) $ scanner

-- projY :: Scanner -> [Int]
-- projY scanner = Set.map (\(Coord _ y _) -> y) $ scanner

-- projZ :: Scanner -> [Int]
-- projZ scanner = Set.map (\(Coord _ _ z) -> z) $ scanner


projX :: Coord -> Int
projX (Coord x _ _) = x

projY :: Coord -> Int
projY (Coord _ y _) = y

projZ :: Coord -> Int
projZ (Coord _ _ z) = z












allRotations :: Scanner -> [Scanner]
allRotations s = map ($s) $ map rotate sl3

offset :: Coord -> Scanner -> Scanner
offset (Coord x y z) scanner = Set.map (\(Coord a b c) -> Coord (a+x) (b+y) (c+z)) scanner


rotate :: Rotation -> Scanner -> Scanner
rotate ((i,j,k), (si,sj,sk)) scanner = Set.map rot scanner
  where
    rot (Coord x y z) = toCoord $ map (\(i,si) -> si * ([x,y,z] !! i)) [(i,si),(j,sj),(k,sk)]
    toCoord [x,y,z] = Coord x y z

orientation :: Rotation -> Bool
orientation (perm, (si,sj,sk)) = (==1) $ (permSign perm) * flipSign
  where
    -- permDist % 4 = 2 -> -1
    -- permDist % 4 = 0 -> 1
    permSign (0,1,2) = 1
    permSign (0,2,1) = -1
    permSign (1,0,2) = -1
    permSign (1,2,0) = 1
    permSign (2,0,1) = 1
    permSign (2,1,0) = -1

    flipSign = si*sj*sk


-- transformations with determinant 1
sl3 :: [Rotation]
sl3 = filter orientation gl3

-- transformations with determinant +-1
gl3 = [((i,j,k), (si,sj,sk)) | [i,j,k] <- perms, sk <- flips, sj <- flips, si <- flips]
  where
    perms = permutations [0,1,2]
    flips = [-1,1]

safeHead [] = Nothing
safeHead xs = Just $ head xs

-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parser PuzzleInput
puzzle = many1 (scannerP <* end)

--example
scannerP :: Parser Scanner
scannerP = do
  -- Parse the title line
  string "--- scanner "
  scannerID <- integer
  string " ---"
  endOfLine


  points <- many1 (coordP <* end)
  return $ Set.fromList points

coordP :: Parser Coord
coordP = do
  x <- integer
  char ','
  y <- integer
  char ','
  z <- integer
  return $ Coord x y z









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