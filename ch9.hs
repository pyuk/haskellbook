import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool x y
  | x < y = [x, y]
  | x == y = [x]
  | otherwise = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y | x < y = x : eftOrd (succ x) y
           | x > y = []
           | otherwise = x : []

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x <= y = x : eftInt (x + 1) y
  | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar x y
  | x <= y = x : eftChar (succ x) y
  | otherwise = []

wordy :: String -> [String]
wordy [] = []
wordy xs = takeWhile (/= ' ') xs : wordy (drop 1 $ dropWhile (/= ' ') xs)

myLines :: String -> [String]
myLines [] = []
myLines xs = takeWhile (/= '\n') xs : myLines (drop 1 $ dropWhile (/= '\n') xs)

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
          ++ thirdSen ++ fourthSen
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

inCommon :: Char -> String -> [String]
inCommon _ [] = []
inCommon a xs = takeWhile (/= a) xs : inCommon a (drop 1 $ dropWhile (/= a) xs)

wordy' :: String -> [String]
wordy' xs = inCommon ' ' xs

myLines' :: String -> [String]
myLines' xs = inCommon '\n' xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = x `f` y : zipWith' f xs ys

zip'' :: [a] -> [b] -> [(a,b)]
zip'' xs ys = zipWith' (,) xs ys

capito :: String -> String
capito (x:xs) = toUpper x : xs

cap :: String -> String
cap [] = []
cap (x:xs) = toUpper x : cap xs

headF :: String -> Char
headF = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = a == x || myElem a xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' a xs = myAny (==a) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f 

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x1:x2:xs)
  | x1 `f` x2 == GT = myMaximumBy f (x1:xs)
  | otherwise = myMaximumBy f (x2:xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f (x1:x2:xs)
  | x1 `f` x2 == LT = myMinimumBy f (x1:xs)
  | otherwise = myMinimumBy f (x2:xs)

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare