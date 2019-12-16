import Control.Monad (liftM, ap)

main = interact $
    show . solve . (map read) . words

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

-- takes initial state and first operation, runs until return code is 99
runToHalt :: Tape -> TapeOp Int -> Tape
runToHalt t0 (TapeOp op)  | r == 99 = t1 -- halt code
                          | otherwise = runToHalt t1 $ opCode r
                          where
                              (r, t1) = op t0 

-- takes initial state and first operation, runs until return code is 99
runToHaltOpCode :: Tape -> TapeOp Int -> Tape
runToHaltOpCode t0 (TapeOp op)  | r == 99 = t1 -- halt code
                          | otherwise = runToHalt t1 $ opCode r
                          where
                              (r, t1) = op t0 

opCode :: Int -> TapeOp Int
opCode 99 = undefined
-- intCode 99 = TapeOp (\t -> ((), t))
opCode 1 = opAdd
opCode 2 = opMult
opCode _ = undefined
-- opCode _ = TapeOp (\t -> (99, t)) -- halt

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

setAtIndex :: [Int] -> Int -> Int -> [Int]
setAtIndex (x:xs) 0 val = val:xs
setAtIndex (x:xs) n val = x:(setAtIndex xs (n-1) val)
setAtIndex [] _ _ = undefined




-- -- Given the input tape, return value at position 0 after halting
solve :: [Int] -> Int
solve xs = head $ fst $ (uncurry runToHalt) $ initialise $ restore xs
-- solve :: [Int] -> Tape
-- solve xs =  (uncurry runToHalt) $ initialise xs
-- solve :: [Int] -> (Int, Tape)
-- solve xs = (uncurry runOp) $ initialise  xs

initialise :: [Int] -> (Tape, TapeOp Int)
initialise xs = ((xs++(repeat 0),0), opCode (xs !! 0))

restore :: [Int] -> [Int]
restore (x:_:_:xs) = x:12:2:xs
restore _ = undefined

restore2 :: [Int] -> Int -> Int-> [Int]
restore2 (x:_:_:xs) noun verb = x:noun:verb:xs
restore2 xs noun verb = restore2 (xs ++ (repeat 0)) noun verb

restoreTape :: TapeOp ()
restoreTape = setTape (\(t,pc) -> (restore t, pc))

