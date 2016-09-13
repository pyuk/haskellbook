import Data.List
data Mood = Blah | Woot deriving (Show, Eq)

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

f :: (a,b) -> (c,d) -> ((b,d),(a,c))
f x y = ((snd x, snd y),(fst x, fst y))

--x = (+)
--g xs = x w 1
--  where w = length xs

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r xs = take 1 xs

co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = f . g

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g x = fst . g . f $ x

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = a == b && a' == b'

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt a') = a == a'
  (==) (TisAString a) (TisAString a') = a == a'
  (==) _ _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = a == a' && b == b'

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) (ThatOne a) (ThisOne a') = a == a'
  (==) (ThisOne a) (ThatOne a') = a == a'

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False

settleDown x = if x == Woot then Blah else x

type Subject = String
type Verb = String
type Object = String
data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

--i :: Num a => a
i :: RealFrac a => a
i = 1.0

--freud :: a -> a
freud :: Int -> Int
freud x = x

myX = 1 :: Int

--sigmund :: Num a => a -> a
sigmund x = myX

--jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

mTH x y z = x * y * z
mTH' x y = \z -> x * y * z
mTH'' x = \y -> \z -> x * y * z
mTH''' = \x -> \y -> \z -> x * y * z

addFive = \x y -> if x > y then y + 5 else x + 5

mflip f x y = f y x

k (x, y) = x
k1 = k ((4-1), 10)
k2 = k (3, True)