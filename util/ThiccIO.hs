import Control.Monad.IO.Class
import Control.Monad (liftM, ap)
import Debug.Trace

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

thiccPutStr :: Thickness -> ThiccIO Thickness ()
thiccPutStr x = ThiccIO $ \buff -> return (buff <> x, ())


newtype Thickness = Thickness String
instance Semigroup Thickness where
  Thickness x <> Thickness y = Thickness $ x++y
instance Monoid Thickness where
  mempty = Thickness ""
  mappend = (<>)
instance Show Thickness where
  show (Thickness x) = x

main = runThicc $ do
  liftIO $ putStr "<"
  thiccPutStr $ Thickness "Hello"
  liftIO $ putStrLn ">"



ttrace x = trace (show x) x