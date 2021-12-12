import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
import Data.List (sort, sortOn)
-- import qualified Data.HashMap.Strict as Map

import Debug.Trace
ttrace x = trace (show x) x

part1 = False

main = do
  input <- getContents
  let result = parse caveGraph "(unknown)" input
  case result of
    (Left err) -> print err
    (Right graph) -> do
      let paths = pathsBetween "start" "end" $ symm graph
      -- mapM (putStrLn . join "-") paths
      let allNodes = unique $ map fst graph
      putStr $ show (length allNodes) ++ " nodes: "
      print allNodes

      putStr $ "\tPart1: "
      putStrLn $ (show $ length $ paths) ++ " paths total"

      putStr $ "Shortest path: "
      putStrLn $ show $ length $ head $ sortOn length $ paths

      let totalPaths = filter (\p -> length (unique p) == length allNodes) paths
      let shortestTotal = head $ sortOn (length . unique) $ totalPaths
      putStrLn $ "Shortest path through all nodes: "
      putStr $ (show $ length shortestTotal) ++ " "
      putStrLn $ show $ shortestTotal

      return ()
      -- putStr $ "Shortest path through all nodes: "
      -- putStrLn $ show $ length $ head $ sortOn length $ paths

type Graph = [Edge]
type Node = String
type Edge = (Node, Node)
type Path = [Node]

pathsBetween :: Node -> Node -> Graph -> [Path]
pathsBetween start end graph = result 
  where
    adj = destinations start graph
    result = if start == end
              then [[end]] -- end is lowercase so can only go there once
              else concat $ map pathsFrom adj -- all the paths out
    
    graph' = if small start
              then removeNode start graph 
              else graph

    pathsFrom next = map (start:) $ pathsBetween next end graph'

removeNode :: Node -> Graph -> Graph
removeNode node graph = filter (\x -> fst x /= node && snd x /= node) graph

-- dunno if `isUpper` exists, I'm on a plane
small :: Node -> Bool
small x = all (`elem` "abcdefghijklmnopqrstuvwxyz") x

destinations :: Node -> Graph -> [Node]
destinations _ [] = []
destinations x edges = map snd $ filter ((==x) . fst) edges

lookupWhere :: (a -> Bool) -> [a] -> Maybe a
lookupWhere p [] = Nothing
lookupWhere p (x:xs)  | p x = Just x
                      | otherwise = lookupWhere p xs

symm :: Graph -> Graph
symm g = g ++ map (\(x,y) -> (y,x)) g

unique :: (Ord a, Eq a) => [a] -> [a]
unique = trim . sort
  where
    trim (x:y:xs) | x == y = trim (x:xs)
                  | otherwise = (x:) $ trim (y:xs)
    trim xs = xs




-- =========================================================
--                             Parsers
-- =========================================================

node = many1 letter

edge = do 
  start <- node 
  char '-'
  end <- node
  (endOfLine >> return ()) <|> eof
  return (start,end)

caveGraph = many1 edge




