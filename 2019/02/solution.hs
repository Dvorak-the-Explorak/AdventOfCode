import Control.Monad (liftM, ap)

main = interact $
    show . solveB . (map read) . words

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
-- getTape t = (t,t) -- Why not define like this
 
 -- given a function that modifies a Tape, create a TapeOp with no return value
setTape :: (Tape -> Tape) -> TapeOp ()
setTape f = TapeOp (\t -> ((), f t))

-- take a tape and an operation, give back the return value and updated state
runOp :: Tape -> TapeOp a -> (a,Tape)
runOp t (TapeOp op) = op t

-- nextOp :: Tape -> TapeOp a
-- nextOp (t,pc) = opCode (t !! pc)

-- takes initial state and IntCode of first operation, runs until return code is 99
runToHalt :: Tape -> Int -> Tape
runToHalt t0 99 = t0
runToHalt t0 code0 = runToHalt t1 code1
                      where
                        TapeOp op = opCode code0
                        (code1, t1) = op t0 

-- get an opcode from its IntCode
opCode :: Int -> TapeOp Int
opCode 1 = opAdd
opCode 2 = opMult
-- opCode 99 = undefined
-- opCode _ = undefined
opCode _ = TapeOp (\t -> (99, t)) -- halt

-- takes current tapeState, returns next opCode and updated tapeState
opAdd :: TapeOp Int
opAdd = TapeOp (\(t0,pc0) -> let
                              s0 = t0 !! (pc0+1) -- where to get first summand
                              s1 = t0 !! (pc0+2) -- where to get second summand
                              dest = t0 !! (pc0+3) -- where to put result
                              val = (t0 !! s0) + (t0 !! s1)
                              t1 = setAtIndex t0 dest val
                              pc1 = pc0 + 4
                             -- in (pc1, (t1,pc1)))
                             in (t1 !! pc1, (t1,pc1)))


-- takes current tapestate, returns next opCode and updated tapestate
opMult :: TapeOp Int
opMult = TapeOp (\(t0,pc0) -> let
                              s0 = t0 !! (pc0+1) -- where to get first operand
                              s1 = t0 !! (pc0+2) -- where to get second operand
                              dest = t0 !! (pc0+3) -- where to put result
                              val = (t0 !! s0) * (t0 !! s1)
                              t1 = setAtIndex t0 dest val
                              pc1 = pc0 + 4
                             in (t1 !! pc1, (t1,pc1)))

-- `setAtIndex xs i val` sets xs[i]=val
setAtIndex :: [Int] -> Int -> Int -> [Int]
setAtIndex (x:xs) 0 val = val:xs
setAtIndex (x:xs) n val = x:(setAtIndex xs (n-1) val)
setAtIndex [] _ _ = undefined


-- -- Given the input tape, return value at position 0 after halting
solveA :: [Int] -> Int
solveA xs = runProgram $ restore xs

-- solve for the correcct noun,verb
solveB :: [Int] -> Int
solveB xs = 100*noun + verb
  where
    (noun,verb) = head [(n,v) | n <- [0..99], v <- [0..99], 19690720 == (runProgram $ restore2 xs n v)]

-- runs the given program, returning the first element after halting
runProgram :: [Int] -> Int
runProgram xs = head $ fst $ runToHalt (initialise xs) (xs !! 0)

-- turns the given list into a Tape by adding trailing zeros and setting the program counter to 0
--  also gets the first operation on the tape
initialise :: [Int] -> Tape
initialise xs = (xs++(repeat 0),0)

-- restore the gravity assist program
restore :: [Int] -> [Int]
restore xs = restore2 xs 12 2

-- restore the program with arbitrary noun and verb
restore2 :: [Int] -> Int -> Int-> [Int]
restore2 (x:_:_:xs) noun verb = x:noun:verb:xs
restore2 xs noun verb = restore2 (xs ++ (repeat 0)) noun verb

-- Monadic version of restore
restoreTape :: TapeOp ()
restoreTape = setTape (\(t,pc) -> (restore t, pc))

