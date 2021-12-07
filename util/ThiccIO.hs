import Control.Monad.IO.Class
import Control.Monad (liftM, ap)
import Debug.Trace

-- #TODO add a ThiccShow class rather than having an arbitrary type (Show s, Monoid s) => s
--    (ie) ThiccShow a where thiccShow :: a -> [String]




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


newtype Thickness = Thickness String
instance Semigroup Thickness where
  Thickness x <> Thickness y = Thickness $ x++y
instance Monoid Thickness where
  mempty = Thickness ""
  mappend = (<>)
instance Show Thickness where
  show (Thickness x) = x


newtype ThiccLine = ThiccLine [String]
instance Semigroup ThiccLine where
  ThiccLine x <> ThiccLine y = ThiccLine $ combineLines x y
instance Monoid ThiccLine where
  mempty = ThiccLine []
  mappend = (<>)
instance Show ThiccLine where
  show (ThiccLine x) = mconcat $ map (++"\n") x


-- zip 2 lists of strings together, 
--    padding shorter lists with spaces
--    padding shorter entries in the first arg with spaces,

combineLines :: [String] -> [String] -> [String]
combineLines s1 s2 = zipWith (++) s1' s2'
  where
    width = if null s1 then 0 else maximum $ map length $ s1
    height = max (length s1) (length s2)
    s1' = map (padRight width) $ padDown height s1
    s2' = padDown height s2

    padRight n [] = take n $ repeat ' '
    padRight n (x:xs) = x : padRight (n-1) xs

    padDown n [] = take n $ repeat ""
    padDown n (x:xs) = x : padDown (n-1) xs


tallString :: String -> ThiccLine
tallString x = ThiccLine $ map (:[]) x

putTall = putThicc . tallString
putTallLn x = do 
  putThicc $ tallString x
  liftIO $ putStrLn ""
-- putTallLn = flip (>>) (liftIO $ putStrLn "") . (putThicc . tallString)

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