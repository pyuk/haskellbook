import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1,return $ Second a),(1,return $ First b)]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= k = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

testNope :: IO ()
testNope = do
  let theTest = undefined :: Nope (Int, String, Int)
  quickBatch $ functor theTest
  quickBatch $ applicative theTest
  quickBatch $ monad theTest

data PhhbtEither b a = Lefts a | Rights b deriving (Eq, Show)

instance Functor (PhhbtEither b) where
  fmap f (Lefts a) = Lefts $ f a
  fmap _ (Rights b) = Rights b

instance Applicative (PhhbtEither b) where
  pure = Lefts
  (Lefts f) <*> (Lefts x) = Lefts $ f x
  (Rights x) <*> _ = Rights x
  _ <*> (Rights x) = Rights x

instance Monad (PhhbtEither b) where
  return = pure
  (Lefts x) >>= k = k x
  (Rights x) >>= _ = Rights x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhbtEither b a) where
  arbitrary = frequency [(1, Lefts <$> arbitrary),(1, Rights <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (PhhbtEither b a) where (=-=) = eq

testPhb :: IO ()
testPhb = do
  let phhbTest = undefined :: PhhbtEither Int (Int, String, Int)
  quickBatch $ functor phhbTest
  quickBatch $ applicative phhbTest
  quickBatch $ monad phhbTest

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure
  (Identity x) >>= k = k x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

testIdentity :: IO ()
testIdentity = do
  let ident = undefined :: Identity (Int, String, Int)
  quickBatch $ functor ident
  quickBatch $ applicative ident
  quickBatch $ monad ident

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs =
    fmap f xs `append` (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= k = Nil
  (Cons x xs) >>= k = k x `append` (xs >>= k)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = Cons <$> arbitrary <*> pure Nil

instance Eq a => EqProp (List a) where (=-=) = eq

testList :: IO ()
testList = do
  let lists = undefined :: List (String, Int, Int)
  quickBatch $ functor lists
  quickBatch $ applicative lists
  quickBatch $ monad lists

j :: Monad m => m (m a) -> m a
j = (>>= id)

l1 :: Monad m => (a -> b) -> m a -> m b
--l1 f = (>>= return . f)
l1 = fmap

a :: Monad m => m a -> m (a -> b) -> m b
a x f = f >>= (<$> x)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
--meh (x:xs) f = f x >>= (\b -> (++) <$> return [b] <*> meh xs f)
--meh (x:xs) f = f x >>= (\b -> (b:) <$> meh xs f)
meh (x:xs) f = f x >>= ((<$> meh xs f) . (:))

flipType :: Monad m => [m a] -> m [a]
flipType = (`meh` id)
