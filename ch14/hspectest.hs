{-# LANGUAGE DeriveGeneric #-}

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import GHC.Generics

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

main = hspec $ do
  describe "half" $ do
    it "x is equal to halfIdentity" $ do
      property $ \x -> x == halfIdentity (x :: Double)
  describe "list" $ do
    it "sort should put in order" $ do
      property $ \x -> listOrdered (sort x :: [Int])
  describe "associative and commutative" $ do
    it "x y z are associative" $ do
      property $ \x y z -> plusAssociative (x :: Int) y z 
    it "x y z are commutative" $ do
      property $ \x y -> plusCommutative (x :: Int) y
  describe "mult associative and commutative" $ do
    it "mult x y z are associative" $ do
      property $ \x y z -> multAssociative (x :: Int) y z 
    it "mult x y z are commutative" $ do
      property $ \x y -> multCommutative (x :: Int) y

numGen :: Gen Int
numGen = coarbitrary (FuncGen 0) arbitrary

data FuncGen = FuncGen Int deriving (Eq, Show, Generic)

instance CoArbitrary FuncGen

--prop_dollar :: (Eq b, Eq a) => (a -> b) -> a -> Bool
prop_dollar :: (FuncGen -> FuncGen) -> FuncGen -> Bool
prop_dollar f x = f x == (f $ x)

prop_quotrem :: Int -> Int -> Bool
prop_quotrem x y
  | y == 0 = True
  | otherwise = (quot x y)*y + (rem x y) == x

prop_divmod :: Int -> Int -> Bool
prop_divmod x y
  | y == 0 = True
  | otherwise = (div x y)*y + (mod x y) == x

prop_exponcom :: Int -> Int -> Bool
prop_exponcom x y = x ^ y == y ^ x 

prop_exponass :: Int -> Int -> Int -> Bool
prop_exponass x y z = (x ^ y) ^ z == x ^ (y ^ z)

runQc :: IO ()
runQc = quickCheck prop_quotrem

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_,False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_rev :: Eq a => [a] -> Bool
prop_rev xs = (reverse . reverse) xs == id xs

multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative x y =
  x * y == y * x

plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative x y =
  x + y == y + x
