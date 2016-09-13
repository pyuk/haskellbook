import Data.List (intersperse)

divideBy :: Integer -> Integer -> Integer
divideBy x y | x < y = 0
             | otherwise = 1 + divideBy (x - y) y

divideBy' :: Integral a => a -> a -> (a, a)
divideBy' x y = go x y 0
  where go num denom count
          | num < denom = (count, num)
          | otherwise = go (num - denom) denom (count + 1)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

sumNum :: (Eq a, Num a) => a -> a
sumNum 0 = 0
sumNum n = n + sumNum (n - 1)

recAdd :: Integral a => a -> a -> a
recAdd _ 0 = 0
recAdd x y = x + recAdd x (y - 1)

data DividedResult = Result Integer | DividedByZero deriving Show

divideBy'' :: Integral a => a -> a -> DividedResult
divideBy'' _ 0 = DividedByZero
divideBy'' x y 
  | x < 0 && y < 0 = Result $ go (-x) (-y) 0
  | x < 0 = Result $ negate $ go (-x) y 0
  | y < 0 = Result $ negate $ go x (-y) 0
  | otherwise = Result $ go x y 0
  where go num denom count
          | num < denom = count
          | otherwise = go (num - denom) denom (count + 1)

mc91 :: (Num a, Ord a) => a -> a
mc91 n | n > 100 = n - 10
       | otherwise = mc91 $ mc91 (n + 11)

digitToWord :: Int -> String
digitToWord = concat . intersperse "-" . map wordNumber . digits

digits :: Int -> [Int]
digits 0 = []
digits n
  | n < 0 = digits (-n)
  | otherwise = digits (n `div` 10) ++ [n `mod` 10] 

wordNumber :: Int -> String
wordNumber n
  | n < 0 = wordNumber (-n)
  | otherwise = (!!) ["zero","one","two","three","four"
                     ,"five","six","seven","eight","nine"] n