newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ pure (pure x)
  EitherT meab <*> EitherT mea =
    EitherT $ (<*>) <$> meab <*> mea

instance Monad m => Monad (EitherT e m) where
  return = pure
  EitherT mea >>= f = EitherT $ mea >>= \v -> case v of
    Left x -> return $ Left x
    Right x -> runEitherT $ f x

swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mea) = mea >>= \v -> case v of
  Left x -> f x
  Right x -> g x

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma
--  fmap f (ReaderT rma) = ReaderT $ \r -> fmap f (rma r)

instance Applicative m => Applicative (ReaderT r m) where
  pure x = ReaderT $ pure (pure x)
  ReaderT fmab <*> ReaderT rma = ReaderT $ (<*>) <$> fmab <*> rma
--  pure x = ReaderT $ \_ -> pure x
--  ReaderT rmab <*> ReaderT rma = ReaderT $ \r -> rmab r <*> rma r

instance Monad m => Monad (ReaderT r m) where
  return = pure
  ReaderT rma >>= k = ReaderT $ \r -> do
    a <- rma r
    (runReaderT . k) a r

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ \s -> fmap (first f) (smas s)
    where first f (a,s) = (f a, s)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  StateT fmabs <*> StateT smas = StateT $ \s -> do
    (ab, s') <- fmabs s
    (a, s'') <- smas s'
    return (ab a, s'')
    
instance Monad m => Monad (StateT s m) where
  return = pure
  smas >>= k = StateT $ \s -> do
    (a,s') <- (runStateT smas) s
    runStateT (k a) s'
