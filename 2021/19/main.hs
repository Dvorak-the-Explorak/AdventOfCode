{-# LANGUAGE TupleSections #-}
import Text.ParserCombinators.Parsec hiding (State, (*))
import Text.Parsec.Char hiding ((*))

import qualified Data.HashSet as Set
import Data.Hashable
import Data.Maybe
import System.Random
import Control.Monad (replicateM, when)

import Data.List (foldl', permutations, sort, reverse, sortOn)
import Helpers (groupsOf, triples)

import Debug.Trace
ttrace x = trace (show x) x

type Set = Set.HashSet



type PuzzleInput = [Scanner]
type Scanner = (Offset, Set Coord)
type Rotation = ((Int,Int,Int), (Int,Int,Int))
type Offset = Coord
type Transform = (Offset, Rotation)

data Coord = Coord {
  x :: Int,
  y :: Int,
  z :: Int
} deriving (Eq)
makeCoord :: (Int,Int,Int) -> Coord
makeCoord (x,y,z) = Coord x y z


instance Show Coord where
  show (Coord x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"
instance Hashable Coord where
  hashWithSalt s (Coord x y z) = hashWithSalt s (x,y,z)
instance Num Coord where
  negate (Coord x y z) = Coord (-x) (-y) (-z)
  (+) (Coord x1 y1 z1) (Coord x2 y2 z2) = Coord (x1+x2) (y1+y2) (z1+z2) 
  (*) (Coord x1 y1 z1) (Coord x2 y2 z2) = Coord (x1*x2) (y1*y2) (z1*z2) 
  fromInteger x = Coord (fromInteger x) (fromInteger x) (fromInteger x)
  abs (Coord x y z) = Coord (abs x) (abs y) (abs z)
  signum (Coord x y z) = if x*y*z == 0 
                            then 0 
                            else
                              if all (==1) [signum x, signum y, signum z] 
                                then 1 
                                else -1

part1 = True

main = do
  vals <- getPuzzleInput

  -- test

  -- mapM print vals
  -- putStrLn ""

  putStr "Part 1: "
  let result1 = solve1 vals
  print $ map length result1
  let allPoints = foldl' Set.union Set.empty $ map (snd) $ concat result1
  print $ Set.size allPoints
  -- mapM print $ sortOn (\(Coord x _ _) -> x) $ Set.toList allPoints
  return ()

  putStr "Part 2: "
  let result2 = solve2 $ concat result1
  print result2


getPuzzleInput :: IO PuzzleInput
getPuzzleInput = do
  input <- getContents
  let parseResult = parse puzzle "(unknown)" input
  case parseResult of
    (Left err) -> fail $ show err
    (Right puzzleInput) -> return puzzleInput

-- solve1 :: PuzzleInput-> Int
-- solve1 scanners = Set.size $ combineAll scanners
solve1 scanners = solve $ map (:[]) scanners
  where 
    solve [] = []
    solve [x] = [x]
    solve xs = let xs' = combineSets xs in
                if length xs' == length xs 
                  then error "Oh no couldn't combine scanners"
                  else trace (show $ map length xs') solve xs'

solve2 :: PuzzleInput -> Int
solve2 scanners = maximum $ map (uncurry scannerDist) [(s1,s2) | s1 <- scanners, s2 <- scanners]


dist :: Coord -> Coord -> Int
dist (Coord x1 y1 z1) (Coord x2 y2 z2) = (abs $ x1-x2) + (abs $ y1-y2) + (abs $ z1-z2)

scannerDist :: Scanner -> Scanner -> Int
scannerDist (c1, _) (c2,_) = dist c1 c2

matchesRequired = 12


combineSets :: [[Scanner]] -> [[Scanner]]
combineSets [] = []
combineSets [set] = [set]
-- combineSets (s:sets) = s':(combineSets sets')
combineSets (s:sets) = s':(combineSets sets')
  where
    (s', sets') = foldl' zippy (s,[]) sets
    zippy (collection, others) next = case getT next of
                                  Nothing -> (collection, next:others)
                                  -- Just (t, match) -> (collection ++ map (followTransform t match) next, others)
-- ==================================    This bit is wrong     vvvvvvvvvvvvv  ===========================================
                                  Just t -> (collection ++ map (transform t) next, others)
-- ==================================    This bit is wrong     ^^^^^^^^^^^^^ ===========================================
    getT set = safeHead $ catMaybes [findTransform x y | x <- s, y <- set]

    followTransform t source target = offset (fst t) $ 
                                      offset (fst source) $ 
                                      rotate (snd t) $ 
                                      offset (negate $ fst source) target


-- findTransform ::  Scanner -> Scanner -> Maybe (Transform, Scanner)
-- findTransform  s1 s2 = (,s2) <$> result
findTransform ::  Scanner -> Scanner -> Maybe Transform
findTransform  s1 s2 = result
  where
    result = safeHead $ filter ((>=matchesRequired) . countMatch) allTransforms

    countMatch :: Transform -> Int
    countMatch t = match s1 $ transform t s2

    allTransforms :: [Transform]
    allTransforms = [(o,r) | r <- sl3, o <- getOffsets (rotate r s2)]

    getOffsets :: Scanner -> [Offset]
    getOffsets s = [origin] ++ [Coord x y z | x <- offsetsX s, y <- offsetsY s, z <- offsetsZ s]

    offsetsX :: Scanner -> [Int]
    offsetsX s = offsets1D projX s1 s
    offsetsY s = offsets1D projY s1 s
    offsetsZ s = offsets1D projZ s1 s


-- How many points match in the overlapping regions,
--  returns 0 if any unmatched points in overlap
match :: Scanner -> Scanner  -> Int
match scan1 scan2 = result
  where
    result = if coords1 == coords2 
              then Set.size coords1 
              else 0
    coords2 = Set.filter (inBox $ fst scan2) $ snd scan1
    coords1 = Set.filter (inBox $ fst scan1) $ snd scan2
 


inBox :: Coord -> Coord -> Bool
inBox (Coord x y z) (Coord cx cy cz) = xin && yin && zin
  where
    xin = abs (cx - x) <= 1000
    yin = abs (cy - y) <= 1000
    zin = abs (cz - z) <= 1000

origin = Coord 0 0 0

offsets1D :: (Coord -> Int) -> Scanner -> Scanner -> [Int]
offsets1D proj s1 s2 = filter twelve [minOffset..maxOffset]
  where
    -- lists of projected coordinates
    s1' = sort $ map proj $ Set.toList $ snd s1
    s2' = sort $ map proj $ Set.toList $ snd s2

    maxOffset = last s1' - head s2'
    minOffset = head s1' - last s2'

    twelve off = (>=matchesRequired) $ countMatches s1' $ map (+off) s2'

    countMatches _ [] = 0
    countMatches [] _ = 0
    countMatches (x:xs) (y:ys) 
      | x == y = 1 + countMatches xs ys
      | x < y = countMatches xs (y:ys)
      | otherwise = countMatches (x:xs) ys



projX :: Coord -> Int
projX (Coord x _ _) = x

projY :: Coord -> Int
projY (Coord _ y _) = y

projZ :: Coord -> Int
projZ (Coord _ _ z) = z







offset :: Coord -> Scanner -> Scanner
offset o (center, scanner) = (center+o, Set.map (+o) scanner)


rotate :: Rotation -> Scanner -> Scanner
rotate r1@((i,j,k), (si,sj,sk)) (center, scanner) = (rot center, Set.map rot scanner)
  where
    rot (Coord x y z) = toCoord $ map (\(i,si) -> si * ([x,y,z] !! i)) [(i,si),(j,sj),(k,sk)]
    toCoord [x,y,z] = Coord x y z

transform :: Transform -> Scanner -> Scanner
transform (o,r) = offset o . rotate r 

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
  return $ (origin, Set.fromList points)

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




-- =========================================================
--                             Tests
-- =========================================================




test = do
  scanner <- randomScanner 30

  testGeometry scanner

  putStrLn "Tests passed."


testGeometry scanner = do
  testCoord
  testOffsets scanner
  testOffsetRotation scanner
  return ()


testCoord = do
  c1 <- randomCoord
  c2 <- randomCoord

  let r1 = c1 + c2
  let r2 = c2 + c1

  when (r1 /= r2) $ fail $ "Coord addition not commutative!\nc1: " ++ show c1 ++ 
                          "\nc2: " ++ show c2 ++ 
                          "\nc1+c2: " ++ show r1 ++ 
                          "\nc2+c1: " ++ show r2

  when (c1 + (-c1) /= origin) $ fail $ "Coord negate isn't additive inverse!\nc1: " ++ show c1 ++ 
                          "\nc2: " ++ show (-c1) ++ 
                          "\nc1+c2: " ++ show (c1 + (-c1)) ++ 
                          "\nc2+c1: " ++ show (c1 - c1)
  return ()


testOffsetRotation scanner = do
  off <- randomCoord
  let offsetRotate = map (\r -> rotate r $ offset off scanner) sl3 :: [Scanner]
  let rotateOffset = map (\r -> offset off $ rotate r scanner) sl3 :: [Scanner]

  when (any id $ zipWith (/=) offsetRotate rotateOffset) $ fail $ show "Offset and rotation doesn't commute"



testOffsets scanner = do
  offset1 <- randomCoord
  offset2 <- randomCoord

  let s0 = offset (-offset1) $ offset (offset1) scanner
  let s1 = offset offset2 $ offset offset1 scanner
  let s2 = offset offset1 $ offset offset2 scanner
  let s3 = offset (offset2 + offset1) scanner

  let offsetsDescription = "o1: " ++ show offset1 ++ "\no2: " ++ show offset2 ++ "\n"

  when (s0 /= scanner) $ fail $ "Offset of inverse not inverse of offset!\n" ++ 
                            show scanner ++ "\n\n" ++ 
                            show offset1 ++ "\n\n" ++ 
                            show s0
  when (s1 /= s2) $ fail $ "Offsets not commutative!\n" ++ 
                            show scanner ++ "\n\n" ++ 
                            offsetsDescription ++ "\n\n" ++ 
                            show s1 ++ "\n\n" ++ 
                            show s2
  when (s1 /= s3) $ fail $ "Offset doesn't distribute over addition!\n"++ 
                            show scanner ++ "\n\n" ++ 
                            offsetsDescription ++ "\n\n" ++ 
                            show s1 ++ "\n\n" ++ 
                            show s3
  return ()


randomCoord :: IO Coord
randomCoord = do
  [x,y,z] <- map ((subtract 1) . (`mod` 2000)) <$> replicateM 3 (getStdRandom random) :: IO [Int]
  return $ Coord x y z
    

randomScanner :: Int -> IO Scanner
randomScanner count = do
  coords <- replicateM count randomCoord
  return (origin, Set.fromList coords)


