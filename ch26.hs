import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

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

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
--  fmap f (StateT smas) = StateT $ \s -> fmap (first f) (smas s)
  fmap f (StateT smas) = StateT $ (fmap . fmap) (first f) smas
    where first f (a,s) = (f a, s)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ pure . (,) x
  StateT fmabs <*> StateT smas = StateT $ \s -> do
    (ab, s') <- fmabs s
    (a, s'') <- smas s'
    return (ab a, s'')
    
instance Monad m => Monad (StateT s m) where
  return = pure
  smas >>= k = StateT $ \s -> do
    (a,s') <- (runStateT smas) s
    runStateT (k a) s'

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance Functor m => Functor (ExceptT e m) where
  fmap f (ExceptT mea) = ExceptT $ (fmap . fmap) f mea

instance Applicative m => Applicative (ExceptT e m) where
  pure x = ExceptT $ pure (Right x)
  ExceptT fmeab <*> ExceptT mea = ExceptT $ (<*>) <$> fmeab <*> mea

instance Monad m => Monad (ExceptT e m) where
  return = pure
  ExceptT mea >>= k = ExceptT $ do
    v <- mea
    case v of
      Right x -> runExceptT (k x)
      Left x -> return $ Left x

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT . ExceptT . ReaderT $ return . const (Right (Just 1))

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

instance MonadTrans (StateT s) where
  lift = StateT . (\a s -> a >>= \x -> return (x,s))

data MaybeT' m a = MaybeT' { runMaybeT' :: m (Maybe a) }

instance Functor m => Functor (MaybeT' m) where
  fmap f (MaybeT' x) = MaybeT' $ (fmap . fmap) f x

instance Applicative m => Applicative (MaybeT' m) where
  pure = MaybeT' . pure . pure
  MaybeT' f <*> MaybeT' x = MaybeT' $ (<*>) <$> f <*> x

instance Monad m => Monad (MaybeT' m) where
  return = pure
  MaybeT' x >>= k = MaybeT' $ x >>= \x' -> case x' of
    Just a -> runMaybeT' . k $ a
    Nothing -> return Nothing

instance MonadTrans MaybeT' where
  lift = MaybeT' . fmap Just

instance (MonadIO m) => MonadIO (MaybeT' m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = ReaderT . const . liftIO

instance MonadIO m => MonadIO (StateT r m) where
  liftIO = StateT . (\a s -> a >>= \x -> return (x,s)) . liftIO

instance MonadIO m => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

data Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader x) = Reader $ fmap f x

instance Applicative (Reader r) where
  pure x = Reader $ \_ -> x
  Reader f <*> Reader x = Reader $ f <*> x

instance Monad (Reader r) where
  return = pure
  Reader x >>= k = Reader $ \r -> runReader (k $ x r) r

rDec :: Num a => Reader a a
rDec = Reader $ (`subtract` 1)

data Identity a = Identity a deriving Show

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \r -> Identity (show r)
