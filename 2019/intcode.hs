{-# LANGUAGE DeriveFunctor #-}


-- module Intcode where

import Control.Monad (liftM, ap)

main = interact $
    show . debug . (map read) . words

-- debug :: [Int] -> [Int]
-- debug _ = getVals ([1,2,3,4,2,2,2], 0) [1]


debug :: [Int] -> [Int]
debug xs = take 10 $ tape $ runOp runToHalt $ tapeFromList xs


debug2 :: [Int] -> [Int]
debug2 xs = take 10 $ tape $ runOp doNextOp $ tapeFromList xs



-- "State of the system" tape and program counter
-- type Tape = ([Int], Int)

data Tape = Tape {tape::[Int], pc::Int}

-- operations on a tape which also produce values (of type a)
--      monadic type
data TapeOp a = TapeOp (Tape -> (a,Tape))
    deriving (Functor)

--hecc what is an Applicative?  looks like pure is just return anyway
--  and someone on StackExchange said this will work (https://stackoverflow.com/a/31652592/2368900)
instance Applicative TapeOp where
  pure = return
  (<*>) = ap

instance Monad TapeOp where
    -- (>>=) :: TapeOp a -> (a -> TapeOp a) -> TapeOp a
    -- getOp2 :: a -> TapeOp a (give me a result type and I'll give you a TapeOp)
    TapeOp op1 >>= getOp2 = TapeOp (\t0 ->  let 
                                             (r1, t1) = op1 t0  -- result of first operation and intermediate tape state
                                             TapeOp op2 = getOp2 r1 -- second operation selected from result of first
                                            in op2 t1) -- result and tape state from second operation
    return r = TapeOp (\t0 -> (r, t0)) -- don't modify the tape, just return the value given

 -- return value is the tape itself, tape is not modified
getTape :: TapeOp Tape
getTape = TapeOp (\t -> (t,t))
-- getTape t = (t,t) -- Why not define like this (monad something something?)
 

getVal :: Tape -> Int -> Int
getVal t n = (tape t) !! n 

-- Takes a tape and a list of parameter modes, pulls out the parameter values
getVals :: Tape -> [Int] -> [Int]
getVals _t [] = []
getVals t (m:ms) = case m of
    0 -> relP:(getVals (inc t) ms)
    1 -> dirP:(getVals (inc t) ms)
  where
    relP = (tape t) !! dirP 
    dirP = (tape t) !! ((pc t)+1)

getModes :: Int -> [Int]
getModes x = revDigits (div x 100) -- will create infinite list of 0 at the end
  where
    revDigits x = (mod x 10):(revDigits (div x 10))

getParams :: Tape -> [Int]
getParams t = getVals t $ getModes (curr t)



 -- given a function that modifies a Tape, create a TapeOp with no return value
setTape :: (Tape -> Tape) -> TapeOp ()
setTape f = TapeOp (\t -> ((), f t))

-- take a tape and an operation, give back just the updated state
runOp :: TapeOp a -> Tape -> Tape
runOp (TapeOp op) t  = snd $ op t

-- take a tape and an operation, give back the return value and updated state
applyOp :: TapeOp a -> Tape -> (a,Tape)
applyOp (TapeOp op) t  = op t

nextOp :: Tape -> TapeOp Int
nextOp t = opCode $ curr t

inc :: Tape -> Tape
inc t = Tape { tape = tape t, pc = (pc t) + 1}

curr :: Tape -> Int
curr t = (tape t) !! (pc t)

-- read the tape to find the opcode
doNextOp :: TapeOp Int
doNextOp = TapeOp $ \t -> applyOp (return (curr t) >>= opCode) t
-- doNextOp = TapeOp $ \(t,pc) -> applyOp (return (t !! pc) >>= opCode) (t,pc)


-- just a wrapper for recursiveRunCode
runToHalt :: TapeOp Int
runToHalt = recursiveRunCode 0
  where
    recursiveRunCode 0 = doNextOp >>= recursiveRunCode -- find first intcode from the tape
    recursiveRunCode 99 = return 99
    recursiveRunCode n = (opCode n) >>= recursiveRunCode 

-- get an opcode from its IntCode
opCode :: Int -> TapeOp Int
opCode x = opCode' (mod x 100)
  where 
    opCode' 1 = opAdd
    opCode' 2 = opMult
    -- opCode 99 = undefined
    -- opCode _ = undefined
    opCode' _ = TapeOp (\t -> (99, t)) -- halt

-- uses (n+1) parameters from the tape
nAryOp :: ([Int] -> (Int, [Int])) -> TapeOp Int
nAryOp f = TapeOp (\t -> let
                          vals = getParams t
                          (result, vals') = f vals
                          t1 = setAtIndex (tape t) (head vals') result
                          pc1 = (pc t) + 4

                        in (t1 !! pc1, Tape t1 pc1)) 

binOp :: (Int -> Int -> Int) -> TapeOp Int
binOp f = nAryOp $  \xs -> 
                        ( f (xs !! 0) (xs !! 1), 
                          drop 2 xs )

opAdd = binOp (+)
opMult = binOp (*)

-- `setAtIndex xs i val` sets xs[i]=val
setAtIndex :: [Int] -> Int -> Int -> [Int]
setAtIndex (x:xs) 0 val = val:xs
setAtIndex (x:xs) n val = x:(setAtIndex xs (n-1) val)
setAtIndex [] _ _ = undefined

-- runs the given program, returning the first element after halting
-- runProgram :: [Int] -> Int
-- runProgram xs = head $ fst $ (uncurry runToHalt) $ (tapeFromList xs)
runProgram :: [Int] -> Int
runProgram xs = head $ tape $ runOp runToHalt (tapeFromList xs)

-- turns the given list into a Tape by adding trailing 99s and setting the program counter to 0
tapeFromList :: [Int] -> Tape
tapeFromList xs = Tape (xs++(repeat 99)) 0
