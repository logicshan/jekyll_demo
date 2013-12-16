{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, OverlappingInstances, FlexibleInstances #-}

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer

data Expr = Val Int
          | Div Expr Expr
          deriving (Show)

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

ok = Div (Div (Val 1972) (Val 2)) (Val 23)
err = Div (Val 2) (Div (Val 1) (Div (Val 2) (Val 3)))

evalMaybe :: Expr -> Maybe Int
evalMaybe (Val n) = return n
evalMaybe (Div x y) = do n <- evalMaybe x
                         m <- evalMaybe y
                         if m == 0
                           then Nothing
                           else return (n `div` m)

data Exc a = Raise String
           | Result a
           deriving (Show)

instance Monad Exc where
  -- (>>=) :: Exc a -> (a -> Exc b) -> Exc b
  (Raise s)  >>= _ = Raise s
  (Result x) >>= f = f x
  -- return :: a -> Exc a
  return = Result

-- throwErrorExc = Raise

-- evalExc :: Expr -> Exc Int
evalExc (Val n) = return n
evalExc (Div x y) = do n <- evalExc x
                       m <- evalExc y
                       if m == 0
                         then throwErrorExc $ errorS y m
                         else return $ n `div` m

errorS y m = "Error dividing by " ++ show y ++ " = " ++ show m

type StateST = Int
newtype ST a = S (StateST -> (a, StateST))

instance Monad ST where
  -- return :: a -> ST a
  return x = S $ \s -> (x, s)
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  (S st) >>= f = S $ \s -> let (x, s') = st s
                               S st' = f x
                           in st' s'

-- getST = S (\s -> (s, s))
-- putST s = S (const ((), s))

tickST = do n <- getST
            putST (n+1)

-- evalST :: Expr -> ST Int
evalST (Val n) = return n
evalST (Div x y) = do n <- evalST x
                      m <- evalST y
                      if m == 0
--                        then throwErrorExc "AAARRCHGGG!!"
                        then return n
                        else do {tickST; return (n `div` m)}

instance Show a => Show (ST a) where
  show (S st) = "value: " ++ show x ++ ", count: " ++ show s
    where (x, s) = st 0

class Monad m => MonadExc m where
  throwErrorExc :: String -> m a

instance MonadExc Exc where
  throwErrorExc = Raise

class Monad m => MonadST m where
  runStateST :: m a -> StateST -> m (a, StateST)
  getST :: m StateST
  putST :: StateST -> m ()

instance MonadST ST where
  runStateST (S f) = return . f
  getST = S (\s -> (s, s))
  putST s' = S (const ((), s'))

evalMega ::  (MonadExc m, MonadST m) => Expr -> m Int
evalMega (Val n)   = return n
evalMega (Div x y) = do n <- evalMega x
                        m <- evalMega y
--                        tickST
                        if m == 0 
                          then throwErrorExc $ errorS y m 
                          else do {tickST; return $ n `div` m}

class Transformer t where
  promote :: Monad m => m a -> (t m) a

newtype ExcT m a = MkExc (m (Exc a))

instance Transformer ExcT where
  
  -- promote :: Monad m => m a -> (ExcT m) a
  
  -- MkExc :: m (Exc a) -> Exct m a
  
  -- promote_ :: (Monad m, Monad m1) => m t -> m (m1 t)
  
  -- promote = MkExc . promote_
  
  promote m = MkExc (promote_ m)

promote_ :: (Monad m, Monad m1) => m t -> m (m1 t)
promote_ m = do x <- m
                return $ return x

instance Monad m => Monad (ExcT m) where
  -- return :: a -> Exct m a  ~~  a -> (m (Exc a))
  return = promote . return
  
  -- (>>=) :: ExcT m a -> (a -> ExcT m b) -> ExcT m b ~~ m (Exc a) -> (a -> m (Exc b)) -> m (Exc b)
           
  p >>= f = MkExc $ strip p >>= r
    where r (Result x) = strip $ f x
          r (Raise  s) = return $ Raise s
          strip (MkExc m) = m

instance Monad m => MonadExc (ExcT m) where
  throwErrorExc = MkExc . return . Raise

newtype STT m a = MkSTT (StateST -> m (a, StateST))

instance Transformer STT where
  -- promote :: Monad m => m a -> (t m) a
  -- promote :: Monad m => m a -> (STT m) a
  --                              s -> m (a, s)
  promote m = MkSTT $ \s -> do {x <- m; return (x, s)}

instance Monad m => Monad (STT m) where
  -- return :: a -> STT m a
  return = promote . return

  -- (>>=) :: STT m a -> (a -> STT m b) -> STT m b
  -- (s -> m (a, s)) -> (a -> (s -> m (b, s))) -> (s -> m (b, s))
  m >>= f = MkSTT $ \s -> do (x, s') <- strip m s
                             strip (f x) s'
    where strip (MkSTT f) = f

instance Monad m => MonadST (STT m) where
  --runStateST :: STT m a -> StateST -> STT m (a, StateST)
  --              (s -> m (a, s)) -> s -> (s -> m ((a, s), s))
  runStateST (MkSTT f) s = MkSTT $ \s0 -> do (x, s') <- f s
                                             return ((x, s'), s0)
  -- getST :: STT m StateST
  getST = MkSTT $ \s -> return (s, s)

  -- putST :: StateST -> STT m ()
  putST s = MkSTT (const $ return ((), s))

instance MonadExc m => MonadExc (STT m) where
  -- throwErrorExc :: String -> m a
  -- promote :: Monad m => m a -> (t m) a
  --                       m a -> (STT m) a
  -- throwErrorExc :: String -> (STT m) a
  throwErrorExc = promote . throwErrorExc

instance MonadST m => MonadST (ExcT m) where
  -- Transformer ExcT
  -- promote :: Monad m => m a -> (ExcT m) a
  
  -- MonadST m
  -- getST :: m StateST
  -- putST :: StateST -> m ()
  -- runStateST :: m a -> StateST -> m (a, StateST)
  
  -- MonadST (ExcT m)
  -- getST :: (ExcT m) StateST
  getST = promote getST
  -- putST :: StateST -> (ExcT m) ()
  putST = promote . putST
  -- runStateST :: (ExcT m) a -> StateST -> (ExcT m) (a, StateST)
  --               m (Exc a)                 m (Exc (a, StateST))
  runStateST (MkExc m) s = MkExc $ do (ex, s') <- runStateST m s
                                      case ex of
                                        Result x -> return $ Result (x, s')
                                        Raise err -> return $ Raise err

evalExSt :: Expr -> STT Exc Int
evalExSt = evalMega

evalStEx :: Expr -> ExcT ST Int
evalStEx = evalMega

instance Show a => Show (STT Exc a) where
  show (MkSTT f) = case (f 0) of 
    Raise s         -> "Raise:" ++ s ++ "\n"
    Result (v, cnt) -> "Count:" ++ show cnt ++ "\n" ++
                       "Result: " ++ show v ++ "\n"

instance Show a => Show (ExcT ST a) where
  show (MkExc (S f)) = "Count: " ++ show cnt ++ "\n" ++ show r ++ "\n"
    where (r, cnt) = f 0