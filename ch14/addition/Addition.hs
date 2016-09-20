-- Addition.hs
module Addition where

import Test.Hspec
import Test.QuickCheck

divideBy :: Integral a => a -> a -> (a, a)
divideBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

multBy :: (Eq a, Num a) => a -> a -> a
multBy x 0 = 0
multBy x y = x + multBy x (y - 1)

multByTest :: IO ()
multByTest = hspec $ do
  describe "Multiplication" $ do
    it "9 times 3 is 27" $ do
      multBy 9 3 `shouldBe` 27
    it "3 times 7 is 21" $ do
      multBy 3 7 `shouldBe` 21

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      divideBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      divideBy 22 5 `shouldBe` (4, 2)
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
