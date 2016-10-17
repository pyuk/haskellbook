newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ pure (pure x)
  EitherT meab <*> EitherT mea = EitherT $ (<*>) <$> meab <*> mea

instance Monad m => Monad (EitherT e m) where
  return = pure
  EitherT mea >>= f = EitherT $ do
    v <- mea
    case v of
      Left x -> return $ Left x
      Right x -> runEitherT $ f x

swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mea) = do
  v <- mea
  case v of
    Left x -> f x
    Right x -> g x
