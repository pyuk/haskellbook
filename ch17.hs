import Data.List (elemIndex)
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (liftA3)

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

xs = [1,2,3]
ys = [4,5,6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x' <*> y''

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant x) <*> (Constant y) = Constant $ x `mappend` y

data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons x xs) = x `Cons` take' (n - 1) xs

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = f x `Cons` fmap f xs

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  (Cons f xs) <*> (Cons y ys) = fmap f (Cons y ys) `append` (xs <*> Cons y ys)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    return $ Cons a Nil

instance Eq a => EqProp (List a) where
  (=-=) = eq

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

append' :: ZipList' a -> ZipList' a -> ZipList' a
append' (ZipList' xs) (ZipList' ys) = ZipList' $ xs `append` ys

toMyList :: [a] -> List a
toMyList [] = Nil
toMyList (x:xs) = Cons x (toMyList xs)

instance Applicative ZipList' where
  pure x = ZipList' (x `Cons` Nil)
  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' (Cons f fs)) <*> (ZipList' (Cons x xs)) =
    ZipList' (Cons (f x) Nil) `append'` (ZipList' fs <*> ZipList' xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    return $ ZipList' (Cons a Nil)

data Validation e a = Failure e | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Success f) <*> (Success x) = Success $ f x
  (Failure x) <*> (Failure y) = Failure $ x `mappend` y
  _ <*> (Failure x) = Failure x
  (Failure x) <*> _ = Failure x

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    frequency [(1, return $ Success a), (1,return $ Failure e)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

data Errors =
    DivideByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

pureList :: a -> [a]
pureList = pure

applyList :: [(a -> b)] -> [a] -> [b]
applyList = (<*>)

pureIO :: a -> IO a
pureIO = pure

applyIO :: IO (a -> b) -> IO a -> IO b
applyIO = (<*>)

pureTup :: Monoid b => a -> (b, a)
pureTup = pure

applyTup :: Monoid b => (b, (a -> b)) -> (b, a) -> (b, b)
applyTup = (<*>)

pureArrow :: a -> (->) e a
pureArrow = pure

applyArrow :: (->) e (a -> b) -> (->) e a -> (->) e b
applyArrow = (<*>)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two x g) <*> (Two x' y) = Two (x `mappend` x') (g y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a b f) <*> (Three a' b' x) = Three (a `mappend` a') (b `mappend` b') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' a f g) <*> (Three' a' x y) = Three' (a `mappend` a') (f x) (g y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq
  
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four a b c f) <*> (Four a' b' c' x) =
    Four (a `mappend` a') (b `mappend` b') (c `mappend` c') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c x) = Four' a b c (f x)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' a b c f) <*> (Four' a' b' c' x) =
    Four' (a `mappend` a') (b `mappend` b') (c `mappend` c') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos xs ys zs = liftA3 (,,) xs ys zs
