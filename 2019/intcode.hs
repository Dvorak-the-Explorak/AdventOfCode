module Intcode (Tape,IntComp,icBoot,icRun) where

{-# LANGUAGE DeriveFunctor, GADTs #-}
import Data.Maybe





type Tape = [Maybe Int]

data IntComp = IntComp {
  tape :: Tape,
  pc :: Int,
  inputs :: [Int],
  outputs :: [Int]
}

instance Show IntComp where
  show (IntComp t pc i o) =    "tape: " ++ (show $ highlight pc t') ++ "\n"
                            ++ "in:   " ++ (show i) ++ "\n" 
                            ++ "out:  "++ (show $ reverse o)
    where
      t' = map prettyMaybe relevantTape
      relevantTape = pre ++ takeWhile isJust suf
      (pre,suf) = splitAt (pc+1) t


-- class for modifying the show method of an existing type
data Lit a = Lit (String -> String) a

instance Show a => Show (Lit a) where
  show (Lit f x) = f $ show x

prettyMaybe :: Show a => Maybe a -> Lit (Maybe a)
prettyMaybe x | isJust x = Lit (drop 5) x
              | otherwise = Lit (\_ -> "?") x


highlight :: Show a => Int -> [a] -> [Lit a]
highlight n xs = pre' ++ [target'] ++ suf'
  where
    (pre,target:suf) = splitAt n xs
    pre' = map (Lit id) pre
    suf' = map (Lit id) suf
    target' = Lit (\s -> "<"++s++">") target

-- main = interact $
--     show . run2 . (map read) . words


run1 :: [Int] -> [Int]
run1 = justPrefix . tape . icRun . flip icBoot []

run2 :: [Int] -> IntComp
run2 = icRun . flip icBoot []



takeWhileAtLeast :: (a -> Bool) -> Int -> [a] -> [a]
takeWhileAtLeast pred n xs = pre ++ takeWhile pred suf
  where
    (pre,suf) = splitAt n xs


justPrefix :: [Maybe Int] -> [Int]
justPrefix = map fromJust . takeWhile isJust

icBoot :: [Int] -> [Int] -> IntComp
icBoot t inputs = IntComp ((map Just t) ++ repeat Nothing) 0 inputs []


icHalted :: IntComp -> Bool
icHalted c = case tape c !! pc c of
  Just 99 -> True
  Nothing -> True
  _ -> False

icStep :: IntComp -> IntComp
icStep c@(IntComp tape pc ins outs) = jumpPc $ icOp opcode c $ justPrefix args
  where
    opcode :: Int
    opcode  | isJust x = fromJust x
            | otherwise = error $ "Null opcode:\n" ++ show c
      where 
        x = (tape !! pc)
    jumpPc (IntComp t pc i o) = IntComp t pc' i o
    pc' = icOpJumps opcode pc
    args = readArgs modes $ pc+1
    modes = icModes opcode
    -- local function to closure 'tape' in
    readArgs (0:ms) i = (tape !! curr):(readArgs ms (i+1)) -- relative
      where
        curr  | isJust (tape !! i)  = fromJust (tape !! i)
              | otherwise           = error $ "Null pointer dereference in relative argument:\n" ++ show c
    readArgs (1:ms) i = (tape !! i):(readArgs ms (i+1)) -- direct

icOp :: Int -> IntComp -> [Int] -> IntComp
icOp = icOp' . (flip mod 100)
  where 
    icOp' 1 = \c (x:y:d:_) -> icSetTapeAt d (x+y) c -- add
    icOp' 2 = \c (x:y:d:_) -> icSetTapeAt d (x*y) c -- multiply
    icOp' 3 = \c@(IntComp t pc (i:is) o) (d:_) -> icSetTapeAt d (i) c -- take input
    icOp' 4 = \(IntComp t pc i o) (val:_) -> IntComp t pc i (val:o)

icSetTapeAt :: Int -> Int -> IntComp -> IntComp
icSetTapeAt n val (IntComp t pc i o) = IntComp (setAtIndex n (Just val) t) pc i o

icRun :: IntComp -> IntComp
icRun c | icHalted c = c
        | otherwise = icRun $ icStep c

-- takes an opcode, gives a function to update the PC 
icOpJumps :: Int -> Int -> Int
icOpJumps 1 = (+4)
icOpJumps 2 = (+4)
icOpJumps 3 = (+2)
icOpJumps 4 = (+2)
icOpJumps _op = (+4)





-- will create infinite list of 0 at the end
icModes :: Int -> [Int]
icModes x = revDigits (x `div` 100)
  where
    revDigits x = (x `mod` 10):(revDigits (x `div` 10))


-- `setAtIndex xs i val` sets xs[i]=val
setAtIndex :: Int -> a -> [a] -> [a]
setAtIndex n val xs = pre ++ [val] ++ suf
  where
    (pre,_:suf) = splitAt n xs
