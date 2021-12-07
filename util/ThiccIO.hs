import Control.Monad.IO.Class
import Control.Monad (liftM, ap)
import Debug.Trace

newtype ThiccIO a = ThiccIO (String -> IO (String,a))

-- Sequence the two operations together
instance Monad ThiccIO where
  return x = ThiccIO $ \buff -> return (buff, x)
  ThiccIO f1 >>= getNext = 
    ThiccIO $ \buff1 -> do
                (buff2, x1) <- f1 buff1
                let ThiccIO f2 = getNext x1
                f2 buff2

instance Functor ThiccIO where
  fmap = liftM

instance Applicative ThiccIO where
  pure = return
  (<*>) = ap


-- IO operations flush the buffer
instance MonadIO ThiccIO where
  liftIO op = ThiccIO $ \buff -> do
                            putStr buff
                            x <- op
                            return ("", x)

-- running it as IO flushes the buffer at the end
runThicc :: ThiccIO a -> IO a
runThicc (ThiccIO f) = do 
  (buff, x) <- f ""
  putStr buff
  return x

thiccPutStr :: String -> ThiccIO ()
thiccPutStr x = ThiccIO $ \buff -> return (buff++x, ())

main = runThicc $ do
  liftIO $ putStr "<"
  thiccPutStr "Hello"
  liftIO $ putStrLn ">"



ttrace x = trace (show x) x