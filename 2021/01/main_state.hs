import Control.Monad.State
import Data.Tuple.Extra

main = interact $
  show . solve . tripleSums . (map read) . lines


solve :: [Int] -> Int
solve xs = snd $ execState (seqState process xs) (Nothing, 0)

process :: Int -> State (Maybe Int, Int) ()
process n = do
  -- if input is bigger than prev value, add 1 to the count
  whenM (increasing n) addTally
  -- save input as the new "prev value"
  save n

addTally :: State (Maybe Int, Int) ()
addTally = modify $ second (+1)

increasing :: Int -> State (Maybe Int, Int) Bool
increasing n = gets (maybe False (n>) . fst)

-- can import from Control.Monad with IfElse library
whenM :: Monad m => m Bool -> m () -> m ()
whenM cond op = cond >>= (\b -> if b then op else return () )

save :: Int -> State (Maybe Int, Int) ()
-- save n = modify (first $ const $ Just n)
save = modify . first . const . Just


tripleSums :: [Int] -> [Int]
tripleSums xs = zipWith3 add3 xs (tail xs) (drop 2 xs)
  where 
    -- add3 a b c = a + b + c
    add3 = (fmap fmap fmap) (+) (+) --- BAHHhahahah

    -- (+) :: Int -> Int -> Int
    -- (+) :: ((->) Int)  (Int -> Int)
    -- (+) :: ((->) Int)  (((->) Int)  Int)
    -- so (+) is (f f Int) or (f (Int -> Int)) 
    --   where f a is (->) Int a
    --             (functions from Int to a)
    -- fmap :: (a -> b) -> f a -> f b
    -- (for any functor, but from now f will mean (->) Int)
    -- (fmap fmap fmap) (+) (+) 
    --   = (fmap . fmap) (+) (+)
    --   = fmap (fmap (+)) (+)


    -- fmap (+) :: f Int -> f (Int -> Int)
    -- fmap (fmap (+)) :: f f Int -> f f (Int -> Int)
    -- fmap (fmap (+)) (+) :: f f (Int -> Int)
    --                     :: f (((->) Int) (Int -> Int))
    --                     :: f (Int -> Int -> Int)
    --                     :: ((->) Int) (Int -> Int -> Int)
    --                     :: Int -> Int -> Int -> Int


-- run the operation on each of a list of inputs
-- eg seqState op [a,b,c,d,...] = do
                -- op a
                -- op b
                -- op c
                -- op d
                -- ...
seqState :: (a -> State b ()) -> ([a] -> State b ())
seqState _ [] = return ()
seqState op (x:xs) = op x >> seqState op xs
