import Control.Monad.State

import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char
import Data.Tuple.Extra
import Data.List (foldl')

main = do
  input <- getContents
  let result = parse instructions "(unknown)" input
  case result of
    (Left err) -> print err
    (Right instrs) -> do
      let (x,depth) = followAll instrs
      print $ x*depth


data Instruction = Up Int | Down Int | Forward Int
  deriving Show

-- ===============================================================
--                  Parsing
-- ===============================================================

parseInstruction :: String -> Either ParseError [Instruction]
parseInstruction input = parse instructions "(unknown)" input

instructions = many instruction

instruction = do
  many endOfLine
  up <|> down <|> forward


up = do
  string "up "
  Up <$> integer

down = do
  string "down "
  Down <$> integer

forward = do
  string "forward "
  Forward <$> integer


integer = read <$> many1 digit 



-- ===============================================================
--                        Following
-- ===============================================================

type Pos = Int
type Depth = Int
type Aim = Int

followAllState :: [Instruction] -> (Pos,Depth)
followAllState instr = (pos,depth)
  where
    (pos,depth,aim) = execState (seqState followState instr) (0,0,0)

followState :: Instruction ->  State (Pos,Depth,Aim) ()
followState instr = do
  (pos,depth,aim) <- get
  put $ case instr of 
          (Up n) -> (pos,depth,aim-n)
          (Down n) -> (pos,depth,aim+n)
          (Forward n) -> (pos+n,depth+aim*n, aim)

followAll :: [Instruction] -> (Pos,Depth)
followAll xs = (fpos,fdepth)
  where
    (fpos, fdepth, _) = foldl' follow (0,0,0) xs
    follow (pos,depth,aim) instr = case instr of 
                                    (Up n) -> (pos,depth,aim-n)
                                    (Down n) -> (pos,depth,aim+n)
                                    (Forward n) -> (pos+n,depth+aim*n, aim)




seqState :: (a -> State b ()) -> ([a] -> State b ())
seqState _ [] = return ()
seqState op (x:xs) = op x >> seqState op xs