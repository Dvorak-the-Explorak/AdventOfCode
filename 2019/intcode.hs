module Intcode (ICTape,IntComp,icBoot,icRun,icGetTape,icGetPC,
                                icGetInputs,icGetOutputs,icGetStatus) where

{-# LANGUAGE DeriveFunctor, GADTs #-}
import Data.Maybe
import Control.Applicative

type ICTape = [Maybe Int]
data ICStatus = OK | ICError String

data IntComp = IntComp {
  tape :: ICTape,
  pc :: Int,
  inputs :: [Int],
  outputs :: [Int],
  status :: ICStatus
}

icGetTape c = tape c
icGetPC c = pc c 
icGetInputs c = inputs c
icGetOutputs c = tail $ reverse $ outputs c
icGetStatus c = status c


icGetSteps c = last $ outputs c


instance Show IntComp where
  show c@(IntComp t pc i o OK) =    "tape after "++ (show $ icGetSteps c) ++" steps:\n" ++ (show $ highlight pc t') ++ "\n"
                            ++ "in:   " ++ (show i) ++ "\n" 
                            ++ "out:  "++ (show $ icGetOutputs c)
    where
      t' = map prettyMaybe relevantTape
      relevantTape = pre ++ takeWhile isJust suf
      (pre,suf) = splitAt (pc+1) t
  show c@(IntComp t pc i o (ICError s)) = "INTCODE_ERROR:\n" ++ 
                                            s ++ "\n" ++
                                            (show $ c{status = OK})




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
icBoot t inputs = IntComp ((map Just t) ++ repeat Nothing) 0 inputs [0] OK

icHalted :: IntComp -> Bool
isHalted c@(IntComp _ _ _ _ (ICError s)) = True
icHalted c  | pc c < 0 = True
            | otherwise = case tape c !! pc c of
                            Just 99 -> True
                            Nothing -> True
                            _ -> False

icStep :: IntComp -> IntComp
icStep c | icHalted c = c
icStep c@(IntComp tape pc ins outs OK) = addStep $ icOp opcode (zip args modes) c
  where
    opcode :: Int
    opcode  | isJust x = fromJust x
            | otherwise = error $ "Null opcode:\n" ++ show c
      where 
        x = (tape !! pc)
    args = drop (pc+1) tape
    modes = icModes opcode
    addStep ic = ic {outputs = init (outputs ic) ++ [1 + (last $ outputs ic)]}
icStep c = error $ show c







-- #TODO incorporate the Maybe stuff better
icOp :: Int -> [(Maybe Int,Int)] -> IntComp -> IntComp
icOp _ _ c | icHalted c = c
icOp n args c = icOp' (n `mod` 100)
  where 
    parseArg arg 0 = arg >>= (tape c !!) -- '>>=' used to handle Maybe
    parseArg arg 1 = arg 

    p0 = uncurry parseArg $ args !! 0
    p1 = uncurry parseArg $ args !! 1
    d0 = fst $ args !! 0
    d1 = fst $ args !! 1
    d2 = fst $ args !! 2
    p0' = fromJust p0    
    p1' = fromJust p1
    d0' = fromJust d0
    d1' = fromJust d1
    d2' = fromJust d2


    icOp' :: Int -> IntComp
    -- (p0,p1,d2): mem[d2] <- p0+p1
    icOp' 1 | isNothing p0 || isNothing p1 || isNothing d2 = c {status = ICError "Missing parameter!"}
            | otherwise = icJump 4 $ icSetTapeAt d2' (liftA2 (+) p0 p1) c
    -- (p0,p1,d2): mem[d2] <- p0*p1
    icOp' 2 | isNothing p0 || isNothing p1 || isNothing d2 = c {status = ICError "Missing parameter!"}
            | otherwise = icJump 4 $ icSetTapeAt d2' (liftA2 (*) p0 p1) c -- multiply
    -- (d0): mem[d0] <- head input
    icOp' 3 | null $ inputs c = c {status = ICError "No inputs available!"}
            | otherwise       = seq d0 $ icJump 2 
                                      $ icEatInput
                                      $ icSetTapeAt d0' (Just $ head $ inputs c) c  -- take input
    -- (p0): output mem[p0]
    icOp' 4 | isNothing p0 = c {status = ICError "No value to output!"}
            | otherwise = icJump 2 (c {outputs = p0':(outputs c)})
    -- (p0:p1): jgz p0 p1
    icOp' 5 | isNothing p0 || isNothing p1 = c {status = ICError "Missing parameter!"}
            | otherwise = if p0' > 0 
                          then c {pc = p1'} 
                          -- then c {status = ICError $ "p0="++(show p0') ++ " arg0="++(show $ head args)}
                          else icJump 3 c
    -- (p0:p1): jez p0 p1
    icOp' 6 | isNothing p0 || isNothing p1 = c {status = ICError "Missing parameter!"}
            | otherwise = if p0' == 0 
                          then c { pc = p1'} 
                          else icJump 3 c
    -- (p0:p1:d2): mem[d2] = (p0<p1 ? 1 : 0)
    icOp' 7 | isNothing p0 || isNothing p1 || isNothing d2 = c {status = ICError "Missing parameter!"}
            | otherwise  = icJump 4 $ if p0' < p1'
                                      then icSetTapeAt d2' (Just 1) c
                                      else icSetTapeAt d2' (Just 0) c
    -- (p0:p1:d2): mem[d2] = (p0==p1 ? 1 : 0)
    icOp' 8 | isNothing p0 || isNothing p1 || isNothing d2 = c {status = ICError "Missing parameter!"}
            | otherwise = icJump 4 $ if p0' == p1'
                                      then icSetTapeAt d2' (Just 1) c
                                      else icSetTapeAt d2' (Just 0) c

    icOp' n = c {status = ICError ("Error parsing opcode " ++ show n 
                                    ++ ", args=" ++ (show $ take 5 $ map fst args) 
                                    ++ ", modes=" ++ (show $ take 5 $ map snd args))}




icJump :: Int -> IntComp -> IntComp
icJump _ c | icHalted c = c
icJump n c = c {pc = n + pc c}

icSetTapeAt :: Int -> Maybe Int -> IntComp -> IntComp
icSetTapeAt _ _ c | icHalted c = c
icSetTapeAt n val c = c {tape = t'}
  where 
    t' = setAtIndex n val $ tape c

icEatInput :: IntComp -> IntComp
icEatInput c = c {inputs = tail $ inputs c}

icRun :: IntComp -> IntComp
icRun c | icHalted c = c
        | otherwise = icRun $ icStep c
-- icRun c = icRun' 3 c
--   where
--     icRun' 0 c = c
--     icRun' n c  | icHalted c = c
--                 | otherwise = icRun' (n-1) $ icStep c



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

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing