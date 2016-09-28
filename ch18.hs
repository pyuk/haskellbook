import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second x) = Second $ f x
  fmap _ (First x) = First x

instance Applicative (Sum a) where
  pure = Second
  (Second f) <*> (Second x) = Second $ f x
  (First x) <*> _ = First x
  _ <*> (First x) = First x

instance Monad (Sum a) where
  return = pure
  (Second x) >>= k = k x
  (First x) >>= k = First x
