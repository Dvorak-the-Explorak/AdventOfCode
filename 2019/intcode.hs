-- module Intcode where

import Control.Monad (liftM, ap)

main = interact $
    show . debug . (map read) . words

-- debug :: [Int] -> [Int]
-- debug _ = getVals ([1,2,3,4,2,2,2], 0) [1]


debug :: [Int] -> [Int]
debug xs = take 10 $ fst $ runOp runToHalt $ initialise xs



-- "State of the system" tape and program counter
type Tape = ([Int], Int)

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
getVal t n = (fst t) !! n 

-- Takes a tape and a list of parameter modes, pulls out the parameter values
getVals :: Tape -> [Int] -> [Int]
getVals _t [] = []
getVals (tape, pc) (0:ms) = (tape !! (tape !! (pc+1) )):(getVals (tape, pc+1) ms)
getVals (tape, pc) (1:ms) = (tape !! (pc+1) ):(getVals (tape, pc+1) ms)

getModes :: Int -> [Int]
getModes x = revDigits (div x 100) -- will create infinite list of 0 at the end
  where
    revDigits x = (mod x 10):(revDigits (div x 10))

getParams :: Tape -> [Int]
getParams (tape,pc) = getVals (tape,pc) $ getModes (tape !! pc)



 -- given a function that modifies a Tape, create a TapeOp with no return value
setTape :: (Tape -> Tape) -> TapeOp ()
setTape f = TapeOp (\t -> ((), f t))

-- take a tape and an operation, give back the return value and updated state
runOp :: TapeOp a -> Tape -> Tape
runOp (TapeOp op) t  = snd $ op t

applyOp :: TapeOp a -> Tape -> (a,Tape)
applyOp (TapeOp op) t  = op t

nextOp :: Tape -> TapeOp Int
nextOp (t,pc) = opCode (t !! pc)

-- read the tape to find the opcode
doNextOp :: TapeOp Int
doNextOp = TapeOp $ \(t,pc) -> applyOp (return (t !! pc) >>= opCode) (t,pc)
-- doNextOp = TapeOp $ \(t,pc) -> applyOp (return (t !! pc) >>= opCode) (t,pc)

-- compose operation n with all following operations until halt
recursiveRunCode :: Int -> TapeOp Int
recursiveRunCode 0 = doNextOp >>= recursiveRunCode -- find first intcode from the tape
recursiveRunCode 99 = return 99
recursiveRunCode n = (opCode n) >>= recursiveRunCode 

-- just a wrapper for recursiveRunCode
runToHalt :: TapeOp Int
runToHalt = recursiveRunCode 0


-- get an opcode from its IntCode
opCode :: Int -> TapeOp Int
opCode x = opCode' (mod x 100)
  where 
    opCode' 1 = opAdd
    opCode' 2 = opMult
    -- opCode 99 = undefined
    -- opCode _ = undefined
    opCode' _ = TapeOp (\t -> (99, t)) -- halt


-- How to know where pc1 is? 
--  ie how many values does f consume??
-- pureOp :: ([Int] -> Int) -> TapeOp Int
-- pureOp f = TapeOp (\(t0,pc0) -> let
--                               vals = getParams (t0,pc0)
--                               result = (vals !! 0) + (vals !! 1)
--                               t1 = setAtIndex t0 (vals !! 2) result
--                               pc1 = pc0 + 4
--                              in (t1 !! pc1, (t1,pc1))) 

-- uses (n+1) parameters from the tape
nAryOp :: ([Int] -> (Int, [Int])) -> TapeOp Int
nAryOp f = TapeOp (\(t0,pc0) -> let
                                  vals = getParams (t0,pc0)
                                  (result, vals') = f vals
                                  t1 = setAtIndex t0 (head vals') result
                                  pc1 = pc0 + 4
                                in (t1 !! pc1, (t1,pc1))) 

binOp :: (Int -> Int -> Int) -> TapeOp Int
binOp f = nAryOp $  \xs -> 
                        ( f (xs !! 0) (xs !! 1), 
                          drop 2 xs )
                      

-- -- uses 3 parameters from the tape: val1, val2, destination
-- binOp :: (Int -> Int -> Int) -> TapeOp Int
-- binOp f = TapeOp (\(t0,pc0) -> let
--                                 vals = getParams (t0,pc0)
--                                 result = f (head vals) (vals !! 1)
--                                 t1 = setAtIndex t0 (vals !! 2) result
--                                 pc1 = pc0 + 4
--                                in (t1 !! pc1, (t1,pc1))) 

opAdd = binOp (+)
opMult = binOp (*)

-- `setAtIndex xs i val` sets xs[i]=val
setAtIndex :: [Int] -> Int -> Int -> [Int]
setAtIndex (x:xs) 0 val = val:xs
setAtIndex (x:xs) n val = x:(setAtIndex xs (n-1) val)
setAtIndex [] _ _ = undefined


-- -- -- Given the input tape, return value at position 0 after halting
solveA :: [Int] -> Int
solveA xs = runProgram $ restore xs
-- Given the input tape, return value at position 0 after halting
-- solveA :: [Int] -> [Int]
-- solveA xs = take 20 $ fst $ runOp runToHalt $ initialise xs

-- solve for the correcct noun,verb
solveB :: [Int] -> Int
solveB xs = 100*noun + verb
  where
    (noun,verb) = head [(n,v) | n <- [0..99], v <- [0..99], 19690720 == (runProgram $ restore2 xs n v)]

-- runs the given program, returning the first element after halting
-- runProgram :: [Int] -> Int
-- runProgram xs = head $ fst $ (uncurry runToHalt) $ (initialise xs)
runProgram :: [Int] -> Int
runProgram xs = head $ fst $ runOp runToHalt (initialise xs)


-- turns the given list into a Tape by adding trailing zeros and setting the program counter to 0
--  also gets the first operation on the tape
-- initialise :: [Int] -> (Tape, Int)
-- initialise xs = ( (xs++(repeat 0),0) 
initialise :: [Int] -> Tape
initialise xs = (xs++(repeat 0),0)

-- restore the gravity assist program
restore :: [Int] -> [Int]
restore xs = restore2 xs 12 2

-- restore the program with arbitrary noun and verb
restore2 :: [Int] -> Int -> Int-> [Int]
restore2 (x:_:_:xs) noun verb = x:noun:verb:xs
restore2 xs noun verb = restore2 (xs ++ (repeat 0)) noun verb

-- -- Monadic version of restore
-- restoreTape :: TapeOp Int
-- restoreTape = setTape (\(t,pc) -> (restore t, pc))
