{-# LANGUAGE DeriveFunctor, GADTs #-}

type Tape = [Int]

data IntComp = IntComp {
  tape :: Tape,
  pc :: Int,
  inputs :: [Int],
  outputs :: [Int]
}

instance Show IntComp where
  show (IntComp t pc i o) = (show pc) ++ (show $ take n t) ++ "\n"
                            ++ "i" ++ (show i) ++ "o"++ (show o)
    where
      n = max pc $ (+1) $ findEnd 0 0 t
      -- look for 10 '99's in a row
      findEnd start 100 _xs = start
      findEnd start i (99:xs) = findEnd start (i+1) xs
      findEnd start i (_:xs) = findEnd (start+i+1) 0 xs


-- class for highlighting element of a list
data Lit a = Lit (String -> String) a

instance Show a => Show (Lit a) where
  show (Lit f x) = f $ show x




highlight :: Show a => Int -> [a] -> [Lit a]
highlight n xs = pre' ++ [target'] ++ suff'
  where
    (pre,target:suff) = splitAt n xs
    pre' = map (Lit id) pre
    suff' = map (Lit id) suff
    target' = Lit (\s -> "_"++s++"_") target





main = interact $
    show . run2 . (map read) . words


run1 :: [Int] -> [Int]
run1 = take 10 . tape . icRun . flip icBoot []

run2 :: [Int] -> IntComp
run2 = icRun . flip icBoot []





icBoot :: Tape -> [Int] -> IntComp
icBoot t inputs = IntComp (t ++ repeat 99) 0 inputs []


icHalted :: IntComp -> Bool
icHalted c = tape c !! pc c == 99

icStep :: IntComp -> IntComp
icStep c@(IntComp tape pc ins outs) = jumpPc $ icOp opcode c args
  where
    opcode :: Int
    opcode = tape !! pc
    jumpPc (IntComp t pc i o) = IntComp t pc' i o
    pc' = icOpJumps opcode pc
    args = readArgs modes $ pc+1
    modes = icModes opcode
    -- local function to closure 'tape' in
    readArgs (0:ms) i = (tape !! (tape !! i)):(readArgs ms (i+1)) -- relative
    readArgs (1:ms) i = (tape !! i):(readArgs ms (i+1)) -- direct

icOp :: Int -> IntComp -> [Int] -> IntComp
icOp = icOp' . (flip mod 100)
  where 
    icOp' 1 = \c (x:y:d:_) -> icSetTapeAt d (x+y) c -- add
    icOp' 2 = \c (x:y:d:_) -> icSetTapeAt d (x*y) c -- multiply
    icOp' 3 = \c@(IntComp t pc (i:is) o) (d:_) -> icSetTapeAt d (i) c -- take input
    icOp' 4 = \(IntComp t pc i o) (val:_) -> IntComp t pc i (val:o)

icSetTapeAt :: Int -> Int -> IntComp -> IntComp
icSetTapeAt n val (IntComp t pc i o) = IntComp (setAtIndex n val t) pc i o

icRun :: IntComp -> IntComp
icRun c | icHalted c = c
        | otherwise = icRun $ icStep c

-- takes an opcode, gives a function to update the PC 
icOpJumps :: Int -> Int -> Int
icOpJumps _op = (+4)





-- will create infinite list of 0 at the end
icModes :: Int -> [Int]
icModes x = revDigits (x `div` 100)
  where
    revDigits x = (x `mod` 10):(revDigits (x `div` 10))


-- `setAtIndex xs i val` sets xs[i]=val
setAtIndex :: Int -> a -> [a] -> [a]
setAtIndex n val xs = pre ++ [val] ++ suff
  where
    (pre,_:suff) = splitAt n xs
