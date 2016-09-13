import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
filDate (DbDate x) xs = x : xs
filDate _ xs = xs

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr filDate []

filNum :: DatabaseItem -> [Integer] -> [Integer]
filNum (DbNumber x) xs = x : xs
filNum _ xs = xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr filNum []

filRec :: UTCTime -> UTCTime -> UTCTime
filRec a b =
  case compare a b of
    GT -> a
    otherwise -> b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr filRec oldest . filterDbDate
  where oldest = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)

mostRecent' :: [DatabaseItem] -> UTCTime
mostRecent' = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = let dbSum = fromIntegral . sumDb $ xs
               theLength = fromIntegral . length . filterDbNumber $ xs
           in  dbSum / theLength

factorial :: (Num a, Enum a) => [a]
factorial = scanl (*) 1 [2..]

stops = "pbtdkg"
vowels = "aeiou"

threeTups :: [(Char,Char,Char)]
threeTups = [(x,y,z) | x <- stops, y <- vowels, z <- stops]

threeTups' :: [(Char,Char,Char)]
threeTups' = filter ((=='p') . threeFst) threeTups
  where threeFst (x,_,_) = x

nouns = ["dog","cat","turtle","suit","dresser"]
verbs = ["jump","run","sit","punch"]

threeTups'' :: [(String,String,String)]
threeTups'' = [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]

seekritFunc x = (fromIntegral . sum . map length . words $ x) /
                (fromIntegral . length . words $ x)

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
  | x == y = True
  | otherwise = myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (==x)

myElem'' :: Eq a => a -> [a] -> Bool
myElem'' x = foldr (||) False . map (==x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldr1 (\a b -> if f a b == GT then a else b)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldr1 (\a b -> if f a b == LT then a else b)