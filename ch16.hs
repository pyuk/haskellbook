{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import GHC.Arr

a = fmap (+1) $ read "[1]" :: [Int]
b = (fmap . fmap) (++ "lol") (Just ["Hi","Hello"])
c = fmap (*2) (\x -> x - 2)
d = fmap ((return '1' ++) . show) (\x -> [x,1..3])
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
--        changed = fmap read . fmap ("123"++) . fmap show $ ioi
        changed' = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed'

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

quickTest :: IO ()
quickTest = do
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Identity Int)

  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Pair Int)

  quickCheck $ \x -> functorIdentity (x :: Two Int Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Two Int Int)

  quickCheck $ \x -> functorIdentity (x :: Three Int Int Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three Int Int Int)

  quickCheck $ \x -> functorIdentity (x :: Three' Int Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three' Int Int)

  quickCheck $ \x -> functorIdentity (x :: Four Int Int Int Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Four Int Int Int Int)

  quickCheck $ \x -> functorIdentity (x :: Four' Int Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Four' Int Int)

data Possibility a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibility where
  fmap f (Yeppers x) = Yeppers $ f x
  fmap _ LolNope = LolNope

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second x) = Second $ f x
  fmap f (First x) = First x

data Company a b c = DeepBlue a b | Something c

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ (Desk x) = Desk x
  fmap f (Bloor x) = Bloor $ f x
  fmap _ Finance = Finance

data K a b = K a

instance Functor (K a) where
  fmap _ (K x) = K x

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a

instance Functor (Flip K' a) where
  fmap f (Flip (K' x)) = Flip $ K' (f x)

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

data LiftOut f a = LiftOut (f a)

instance Functor f => Functor (LiftOut f) where
  fmap f (LiftOut xs) = LiftOut $ fmap f xs

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa xs ys) = DaWrappa (fmap f xs) (fmap f ys)

data IgnoreOne f g a b = IgnoreOneSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreOneSomething xs ys) = IgnoreOneSomething xs (fmap f ys)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious xs ys zs) = Notorious xs ys (fmap f zs)

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs
  fmap _ Nil = Nil

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat $ f x
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s x) = Print s $ f x
  fmap f (Read g) = Read $ fmap f g
