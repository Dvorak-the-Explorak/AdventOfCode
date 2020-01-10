import Control.Monad (liftM, ap)

main = interact $
    show . solveB . (map read) . words

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
