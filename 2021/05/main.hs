import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
import Control.Monad.State
import Data.List (foldl')
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as Map


part1 = False




main = do
  result <- parse ventLines "(Unknown)" <$> getContents
  case result of
    (Left err) -> print err
    (Right vlines) -> do
      putStrLn "Parsed sucessfully"

      -- if any (not . validLine) vlines
      --   then putStrLn "INVALID LINES:" >> mapM print (filter (not.validLine) vlines) >> return ()
      --   else putStrLn "Lines all valid."

      let orthoLines = if part1 
                          then filter (isHorizontal ||| isVertical) vlines
                          else vlines

      let points = mconcat $ map linePoints orthoLines
      let counts = getCounts points
      print $ length $ filter (>=2) $ Map.elems counts




getCounts :: (Eq a, Hashable a) => [a] -> Map.HashMap a Int
getCounts = foldl' (\ acc x -> tally x acc) $ Map.fromList []
  where
    tally x = Map.insertWith (+) x 1





-- ======================================================

type Coord = (Int,Int)
data VentLine = VentLine {
  start :: Coord,
  end :: Coord
}
instance Show VentLine where
  show (VentLine start end) = show start ++ " -> " ++ show end

dir :: VentLine -> Coord
dir (VentLine (ax,ay) (bx,by)) = (signum $ bx - ax, signum $ by - ay)

-- Assumes line is valid
dist :: VentLine -> Int
dist (VentLine (ax,ay) (bx,by)) = max (abs $ bx - ax) (abs $ by - ay)

linePoints :: VentLine -> [Coord]
linePoints line@(VentLine (sx,sy) (ex,ey)) = 
  [(sx + i*dx,sy + i*dy) | i <- [0..dist line]]
    where (dx,dy) = dir line

isHorizontal :: VentLine -> Bool
isHorizontal (VentLine (sx,sy) (ex,ey)) = sy==ey

isVertical :: VentLine -> Bool
isVertical (VentLine (sx,sy) (ex,ey)) = sx==ex


-- ===============================================
--                    Parsers
-- ===============================================

ventLines :: Parser [VentLine]
ventLines = many $ ventLine <* endOfLine

coord = (,) <$> (integer <* char ',') <*> integer
ventLine = VentLine <$> (coord <* string " -> ") <*> coord


integer :: Parser Int
integer = read <$> many1 digit

-- ==============================================
-- ==============================================


(|||) p q x = (p x) || (q x)