module MonadLect where

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a ->  m b -> m b
  fail :: String -> m a
