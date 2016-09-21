import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)

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
  describe "different folds" $ do
    it "folding cons" $
      property $ \x y -> foldr (:) (x :: [Int]) y == (++) x y

twice f = f . f
fourTimes = twice . twice

prop_cap :: String -> Bool
prop_cap s =
  if (capitalizeWord s == twice capitalizeWord s)
  then fourTimes capitalizeWord s == capitalizeWord s
  else False

prop_sort :: String -> Bool
prop_sort s =
  if (sort s == twice sort s)
  then sort s == fourTimes sort s
  else False

capitalizeWord :: String -> String
capitalizeWord s = map toUpper s

square x = x * x
squareIdentity = square . sqrt

prop_square :: Double -> Bool
prop_square x = squareIdentity x == x

prop_readShow :: Int -> Bool
prop_readShow x = (read (show x)) == x

prop_thing :: Int -> [Int] -> Bool
prop_thing x xs = length (take x xs) == x

prop_fold :: [Int] -> [Int] -> Bool
prop_fold x y = foldr (:) x y == (++) x y

prop_fold2 :: [[Int]] -> Bool
prop_fold2 x = foldr (++) [] x == concat x

prop_dollar :: Int -> Bool
prop_dollar x = (+1) x == ((+1) $ x)

prop_dot :: Int -> Bool
prop_dot x = (\x -> x * 2 + 1) x == ((+1) . (*2)) x

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

data Fool = Fulse | Frue deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

instance Arbitrary Fool where
  arbitrary = foolGen

foolGen' :: Gen Fool
foolGen' = elements [Fulse, Fulse, Frue]
