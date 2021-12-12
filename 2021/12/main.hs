import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
import Data.List (sort, sortOn, foldl')
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
      putStr $ "Part1: "
      putStrLn $ (show $ length $ paths) ++ " paths total"


      let lenientPaths = pathsBetweenLenient [] "start" "end" $ symm graph
      putStrLn $ (show $ length lenientPaths) ++ " lenient paths total"

      -- printPaths $ sort lenientPaths

showPath :: Path -> String
showPath = join "-"

printPaths :: [Path] -> IO ()
printPaths paths = do
  mapM (putStrLn . showPath) paths
  return ()



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


pathsBetweenLenient :: [Node] -> Node -> Node -> Graph -> [Path]
pathsBetweenLenient visited start end graph = result 
  where
    adj = destinations start graph
    result = if start == end
              then [[end]] -- end is lowercase so can only go there once
              else concat $ map (map (start:) . pathsFrom) adj -- all the paths out

    visitedRemoved = foldl' (\ g x -> removeNode x g) graph' visited
    graph' = if start `elem` ["start", "end"] 
              then removeNode start graph
              else graph

    pathsFrom next = if small start
                      then if start `elem` visited -- we've visited start once already
                              then pathsBetween next end visitedRemoved -- visit start a second time, now it's just search like part 1
                              else pathsBetweenLenient (start:visited) next end graph' -- visit start first time
                      else pathsBetweenLenient visited next end graph'

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


-- might already exist
join :: String -> [String] -> String
join sep [] = ""
join sep [x] = x
join sep (x:y:xs) = x ++ sep ++ join sep (y:xs)



-- =========================================================
--                             Parsers
-- =========================================================

node :: Parser Node
node = many1 letter

edge :: Parser Edge
edge = do 
  start <- node 
  char '-'
  end <- node
  (endOfLine >> return ()) <|> eof
  return (start,end)

caveGraph :: Parser Graph
caveGraph = many1 edge