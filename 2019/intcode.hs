module Intcode (Tape,IntComp,icBoot,icRun,icGetTape,icGetPC,icGetInputs,icGetOutputs) where

{-# LANGUAGE DeriveFunctor, GADTs #-}
import Data.Maybe





type Tape = [Maybe Int]

data IntComp = IntComp {
  tape :: Tape,
  pc :: Int,
  inputs :: [Int],
  outputs :: [Int]
}

icGetTape c = tape c
icGetPC c = pc c 
icGetInputs c = inputs c
icGetOutputs c = outputs c


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
icStep c@(IntComp tape pc ins outs) = icOp opcode args modes c
  where
    opcode :: Int
    opcode  | isJust x = fromJust x
            | otherwise = error $ "Null opcode:\n" ++ show c
      where 
        x = (tape !! pc)
    args = drop (pc+1) tape
    modes = icModes opcode

-- #TODO incorporate the Maybe stuff better
icOp :: Int -> [Maybe Int] -> [Int] -> IntComp -> IntComp
icOp = icOp' . (flip mod 100)
  where 
    icOp' 1 ((Just x):(Just y):(Just d):_) (m1:m2:_) c  = icJump 4 $ icSetTapeAt d (x'+y') c -- add
      where
        x' = if m1 == 1 then x else fromJust ((tape c) !! x)
        y' = if m2 == 1 then y else fromJust ((tape c) !! y)
    icOp' 2 ((Just x):(Just y):(Just d):_) (m1:m2:_) c  = icJump 4 $icSetTapeAt d (x'*y') c -- multiply
      where
        x' = if m1 == 1 then x else fromJust ((tape c) !! x)
        y' = if m2 == 1 then y else fromJust ((tape c) !! y)
    icOp' 3 ((Just d):_) _ (IntComp t pc (i:is) o)      = icJump 2 $ icSetTapeAt d i (IntComp t pc is o) -- take input
    icOp' 4 ((Just val):_) (m:_) c@(IntComp t pc i o)   = icJump 2 $ IntComp t pc i (val':o)
      where
        val' = if m == 1 then val else fromJust ((tape c) !! val)
    icOp' 5 ((Just x):(Just y):_) (m1:m2:_) c           = if x' > 0 
                                                            then c { pc = y} 
                                                            else icJump 3 c
      where
        x' = if m1 == 1 then x else fromJust ((tape c) !! x)
        y' = if m2 == 1 then y else fromJust ((tape c) !! y)
    icOp' 6 ((Just x):(Just y):_) (m1:m2:_) c           = if x' == 0 
                                                            then c { pc = y} 
                                                            else icJump 3 c
      where
        x' = if m1 == 1 then x else fromJust ((tape c) !! x)
        y' = if m2 == 1 then y else fromJust ((tape c) !! y)
    icOp' 7 ((Just x):(Just y):(Just d):_) (m1:m2:_) c  = icJump 4 $
                                                            if x' < y'
                                                              then icSetTapeAt d 1 c
                                                              else icSetTapeAt d 0 c
      where
        x' = if m1 == 1 then x else fromJust ((tape c) !! x)
        y' = if m2 == 1 then y else fromJust ((tape c) !! y)
    icOp' 8 ((Just x):(Just y):(Just d):_) (m1:m2:_) c  = icJump 4 $
                                                            if x' == y'
                                                              then icSetTapeAt d 1 c
                                                              else icSetTapeAt d 0 c
      where
        x' = if m1 == 1 then x else fromJust ((tape c) !! x)
        y' = if m2 == 1 then y else fromJust ((tape c) !! y)

    icOp' n _ _ c                                       = error $ "Unknown opcode: " ++ show n ++ "\n"
                                                          ++ show c

icJump :: Int -> IntComp -> IntComp
icJump n c = c {pc = n + pc c}

icSetTapeAt :: Int -> Int -> IntComp -> IntComp
icSetTapeAt n val c = c {tape = t'}
  where 
    t' = setAtIndex n (Just val) $ tape c

icRun :: IntComp -> IntComp
icRun c | icHalted c = c
        | otherwise = icRun $ icStep c
-- icRun c = icRun' 2 c
--   where
--     icRun' 0 c = c
--     icRun' n c  | icHalted c = c
--                 | otherwise = icRun' (n-1) $ icStep c

-- -- takes an opcode, gives a function to update the PC 
-- icOpJumps :: Int -> Int -> Int
-- icOpJumps = icOpJumps' . flip mod 100
--   where 
--     icOpJumps' 1 = (+4)
--     icOpJumps' 2 = (+4)
--     icOpJumps' 3 = (+2)
--     icOpJumps' 4 = (+2)
--     icOpJumps' n = error $ "Unknown jump for opcode " ++ show n





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
