import System.Process
import System.Exit
import Data.List (foldl')
-- import Helpers (chain)


-- example
type PuzzleInput = String


part1 = True

main = do
  vals <- getPuzzleInput

  putStr "Part 1: "
  result1 <- solve1 vals
  print result1

  putStr "Part 2: "
  let result1 = solve2 vals
  print result1

getPuzzleInput :: IO PuzzleInput
getPuzzleInput = getContents 

-- example
solve1 :: PuzzleInput -> IO Int
solve1 input = 


-- example
solve2 :: PuzzleInput -> Int
solve2 = return (-1)

getMD5 :: String -> IO String
getMD5 input = do
  let stdin' = input
  (errCode, stdout', stderr') <- readProcessWithExitCode "md5sum" [] stdin'
  case errCode of
    ExitSuccess -> return stdout'
    _ -> fail stderr'