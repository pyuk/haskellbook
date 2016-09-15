notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a = Just a

replaceThe :: String -> String
replaceThe = unwords . map (\a -> if a == "the" then "a" else a) . words

replaceThe' :: String -> String
replaceThe' = unwords . rep . words
  where rep [] = []
        rep (x:xs) = (if x == "the" then "a" else x) : rep xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = count . words
  where count [x] = 0
        count (x:y:xs) = (if x == "the" && head y `elem` "aeiou" then 1 else 0) + count (y:xs)

countTheBeforeVowel' :: String -> Integer
countTheBeforeVowel' = countTheBeforeVowel'' . words
  where countTheBeforeVowel'' [x] = 0
        countTheBeforeVowel'' (x:y:xs)
          | x == "the" && head y `elem` "aeiou" = 1 + countTheBeforeVowel'' (y:xs)
          | otherwise = countTheBeforeVowel'' (y:xs)

countVowels :: String -> Int
countVowels = foldr (\a b -> if a `elem` "aeiou" then 1 + b else b) 0

countVowels' :: String -> Integer
countVowels' [] = 0
countVowels' (x:xs)
  | x `elem` "aeiou" = 1 + countVowels' xs
  | otherwise = countVowels' xs

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord xs
  | countVowels xs > length xs - countVowels xs = Nothing
  | otherwise = Just (Word' xs)

mkWord' :: String -> Maybe Word'
mkWord' xs = let constan = foldr (\a b -> if a `notElem` vowels then 1 + b else b) 0 xs
                 vow = foldr (\a b -> if a `elem` vowels then 1 + b else b) 0 xs
             in if vow > constan then Nothing else Just (Word' xs)

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just $ intToNat x
  where intToNat 0 = Zero
        intToNat x = Succ $ intToNat (x - 1)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just a) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just a) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just x) = f x
mayybee n _ Nothing = n

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe n Nothing = n

fromMaybe' :: a -> Maybe a -> a
fromMaybe' n x = mayybee n id x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map maybeToList . filter isJust

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr test (Just [])
  where test Nothing _ = Nothing
        test _ Nothing = Nothing
        test (Just a) (Just b) = Just $ a : b

flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe' xs = case filter isNothing xs of
                  [] -> Just $ catMaybes xs
                  otherwise -> Nothing

lefts' :: [Either a b] -> [a]
lefts' = foldr test []
  where test (Left x) b = x : b
        test (Right _) b = b

rights' :: [Either a b] -> [b]
rights' = foldr test []
  where test (Right x) b = x : b
        test (Left _) b = b

partitionsEithers' :: [Either a b] -> ([a],[b])
partitionsEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (\a -> Just $ f a)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
                  Just (a,b) -> a : myUnfoldr f b
                  Nothing -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
               Just (x,y,z) -> Node (unfold f x) y (unfold f z)
               Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold un' n
  where un' 0 = Nothing
        un' x = Just (x-1,n-x,x-1)
