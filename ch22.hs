{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Data.Maybe
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

getDogRM :: Reader Person Dog
getDogRM = Reader dogName >>=
           \x -> Reader address >>=
           \y -> return $ Dog x y

chris :: Person
chris = Person (HumanName "Chris Allen")
        (DogName "Papu")
        (Address "Austin")

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (,) (z' n) (z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

main :: IO ()
main = do
  print $ foldr (&&) True $ sequA 5
  print $ sequA (summed $ fromMaybe (0,0) x1)
  print $ bolt (fromMaybe 0 ys)
