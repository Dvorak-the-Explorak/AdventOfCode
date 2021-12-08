import Control.Monad.IO.Class
import Control.Monad (liftM, ap)
import Debug.Trace

-- #TODO add a ThiccShow class rather than having an arbitrary type (Show s, Monoid s) => s
--    (ie) ThiccShow a where thiccShow :: a -> [String]

-- ThiccIO is basically StateT s IO a (but with the tuple flipped)
newtype ThiccIO s a = ThiccIO (s -> IO (s,a))

-- Sequence the two operations together
instance Monad (ThiccIO s) where
  return x = ThiccIO $ \buff -> return (buff, x)
  ThiccIO f1 >>= getNext = 
    ThiccIO $ \buff1 -> do
                (buff2, x1) <- f1 buff1
                let ThiccIO f2 = getNext x1
                f2 buff2

instance Functor (ThiccIO s) where
  fmap = liftM

instance Applicative (ThiccIO s) where
  pure = return
  (<*>) = ap

-- IO operations flush the buffer
instance (Show s, Monoid s) => MonadIO (ThiccIO s) where
  liftIO op = ThiccIO $ \buff -> do
                            putStr $ show buff
                            x <- op
                            return (mempty, x)

-- running it as IO flushes the buffer at the end
runThicc :: (Show s, Monoid s) => ThiccIO s a -> IO a
runThicc (ThiccIO f) = do 
  (buff, x) <- f mempty
  putStr $ show buff
  return x

putThicc :: (Show s, Monoid s) => s -> ThiccIO s ()
putThicc x = ThiccIO $ \buff -> return (buff <> x, ())

newtype ThiccString = ThiccString [String]
instance Semigroup ThiccString where
  ThiccString x <> ThiccString y = ThiccString $ combineLines x y
instance Monoid ThiccString where
  mempty = ThiccString []
  mappend = (<>)
instance Show ThiccString where
  show (ThiccString x) = mconcat $ map (++"\n") x

class ThiccShow a where
  thiccShow :: a -> ThiccString


-- zip 2 lists of strings together, 
--    padding shorter lists with blank lines
--    padding shorter lines with spaces (including any new blank lines),
combineLines :: [String] -> [String] -> [String]
combineLines s1 s2 = zipWith (++) s1' s2'
  where
    width = if null s1 then 0 else maximum $ map length $ s1
    height = max (length s1) (length s2)
    s1' = map (padRight width) $ padDown height s1
    s2' = padDown height s2

    padRight n xs = rPad n ' ' xs
    padDown n xs = rPad n "" xs

rPad :: Int -> a -> [a] -> [a]
rPad n fill xs = xs ++ (take (n - length xs) $ repeat fill)

tallString :: String -> ThiccString
tallString x = ThiccString $ map (:[]) x

-- | Prints a string vertically
putTall :: String -> ThiccIO ThiccString ()
putTall = putThicc . tallString

-- | Prints a string vertically and ends the Thicc line
putTallLn :: String -> ThiccIO ThiccString () 
putTallLn x = do 
  putThicc $ tallString x
  liftIO $ putStrLn ""
-- putTallLn = flip (>>) (liftIO $ putStrLn "") . (putThicc . tallString)

printTall :: Show a => a -> ThiccIO ThiccString ()
printTall = putTallLn . show

printThicc :: ThiccShow a => a -> ThiccIO ThiccString ()
printThicc x = do 
  putThicc $ thiccShow x
  liftIO $ putStrLn ""

main = runThicc $ do
  -- liftIO $ putStrLn "<thiccprinting>"
  putTall "Hello"  
  putTall " "
  putTallLn "Hi"
  putTall ""
  putTall "This is a long string"
  -- liftIO $ putStrLn "</thiccprinting>"

ttrace x = trace (show x) x
ttrace' tag x = trace (tag ++ show x) x