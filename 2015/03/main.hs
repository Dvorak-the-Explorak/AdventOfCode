import Data.List (foldl')
import qualified Data.HashSet as Set
type Set = Set.HashSet
-- import Helpers (chain)


-- example
type PuzzleInput = String
type Coord = (Int,Int)


part1 = True

main = do
  vals <- getPuzzleInput

  putStr "Part 1: "
  let result1 = solve1 vals
  print result1

  putStr "Part 2: "
  let result1 = solve2 vals
  print result1

getPuzzleInput :: IO PuzzleInput
getPuzzleInput = getContents 


-- example
solve1 :: PuzzleInput-> Int
solve1 directions = length $ Set.toList $ snd $ followDirections directions



followDirections :: String -> (Coord, Set Coord)
followDirections directions = (final, visited)
  where
    (final, visited) = foldl' update ((0,0), Set.fromList [(0,0)]) directions
    update (curr,acc) dir = 
      let next = move dir curr
      in (next,Set.insert next acc)

    move '^' = up
    move '>' = right
    move '<' = left
    move 'v' = down

-- example
solve2 :: PuzzleInput -> Int
solve2 directions = length $ Set.toList $ Set.union santa robo
  where
    (santaDirs, roboDirs) = uninterleave directions
    santa = snd $ followDirections santaDirs
    robo = snd $ followDirections roboDirs




uninterleave (x:y:xs) = (x,y) <:> uninterleave xs
uninterleave _ = ([], [])
    

(<:>) (a,b) (as,bs) = (a:as, b:bs)


up :: Coord -> Coord
up (row,col) = (row-1,col)

down :: Coord -> Coord
down (row,col) = (row+1,col)

left :: Coord -> Coord
left (row,col) = (row,col-1)

right :: Coord -> Coord
right (row,col) = (row,col+1)

