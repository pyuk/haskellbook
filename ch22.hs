{-# LANGUAGE InstanceSigs #-}

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

compose :: [Char] -> [Char]
compose = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  a <- cap
  b <- rev
  return $ (,) a b

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap >>= \x -> rev >>= \y -> return $ (,) x y

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f xs ys = f <$> xs <*> ys

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra
  
instance Applicative (Reader r) where
  pure a = Reader $ \_ -> a
  (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r) (ra r)

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> (runReader . aRb . ra) r r

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

getDogRM :: Person -> Dog
--getDogRM = Reader dogName >>= \x -> Reader address >>= \y -> return $ Dog x y
getDogRM = do
  name <- Reader dogName
  addy <- Reader address
  return $ Dog name addy

chris :: Person
chris = Person (HumanName "Chris Allen")
        (DogName "Papu")
        (Address "Austin")
