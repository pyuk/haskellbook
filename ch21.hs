{-# LANGUAGE FlexibleContexts #-}

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> r = fmap f r

instance Foldable Identity where
  foldMap f (Identity x) = f x
  foldr f a (Identity x) = f x a

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

testId = do
  let ident = undefined :: Identity (Int, Int, String)
  quickBatch $ traversable ident

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty
  (Constant f) <*> (Constant x) = Constant $ f `mappend` x

instance Foldable (Constant a) where
  foldMap _ (Constant x) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = Constant <$> pure x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq
  
testConst = do
  let conss = undefined :: Constant (Int, Int, String) (Int, Int, String)
  quickBatch $ traversable conss

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap f (Yep x) = Yep $ f x
  fmap _ Nada = Nada

instance Applicative Optional where
  pure = Yep
  (Yep f) <*> r = fmap f r
  Nada <*> _ = Nada

instance Foldable Optional where
  foldMap f (Yep x) = f x
  foldMap _ Nada = mempty

instance Traversable Optional where
  traverse f (Yep x) = Yep <$> f x
  traverse _ Nada = pure Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1,return Nada),(1,Yep <$> arbitrary)]

instance Eq a => EqProp (Optional a) where (=-=) = eq

testOp = do
  let optio = undefined :: Optional (String, String, String)
  quickBatch $ traversable optio

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (xs `append` ys)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> r = fmap f r `append` (fs <*> r)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x `mappend` (foldMap f xs)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> (traverse f xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = Cons <$> arbitrary <*> pure Nil

instance Eq a => EqProp (List a) where (=-=) = eq

testList = do
  let lis = undefined :: List (Int, String, String)
  quickBatch $ traversable lis

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b x) = Three a b $ f x

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a b f) <*> (Three a' b' x) = Three (a `mappend` a') (b `mappend` b') (f x)

instance Foldable (Three a b) where
  foldMap f (Three a b x) = f x

instance Traversable (Three a b) where
  traverse f (Three a b x) = (Three a b) <$> f x

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

testThree = do
  let thre = undefined :: Three String String (Int, Int, String)
  quickBatch $ traversable thre

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' a f g) <*> (Three' a' x y) = Three' (a `mappend` a') (f x) (g y)

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = f y `mappend` f z

instance Traversable (Three' a) where
  traverse f (Three' a x y) = Three' a <$> f x <*> f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

testThr' = do
  let thre' = undefined :: Three' String (Int, Int, String)
  quickBatch $ traversable thre'

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S g x) = S (fmap f g) $ f x

instance Applicative n => Applicative (S n) where
  pure x = S (pure x) x
  (S g f) <*> (S g' x) = S (g <*> g') (f x)

instance Foldable n => Foldable (S n) where
  foldMap f (S g x) = (foldMap f g) `mappend` f x

instance Traversable n => Traversable (S n) where
  traverse f (S g x) = S <$> (traverse f g) <*> f x

instance (Applicative n, Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> (pure <$> arbitrary) <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where (=-=) = eq

testS = do
  let ss = undefined :: S Maybe (String, String, String)
  quickBatch $ traversable ss

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Applicative Tree where
  pure = Leaf
  (Node l f r) <*> r' = fmap f r'
  Leaf f <*> r = fmap f r
  Empty <*> _ = Empty

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = (foldMap f l) `mappend` (f x) `mappend` (foldMap f r)

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l x r) = Node <$> (traverse f l) <*> (f x) <*> (traverse f r)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = Node <$> (Leaf <$> arbitrary) <*> arbitrary <*> (Leaf <$> arbitrary)

instance Eq a => EqProp (Tree a) where (=-=) = eq

testTree = do
  let tre = undefined :: Tree (String, String, String)
  quickBatch $ traversable tre
