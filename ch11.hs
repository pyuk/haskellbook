import Data.List
import Data.Char

data Price = Price Integer deriving (Eq,Show)

data Manufacturer = Mini | Mazda | Tata
  deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChangesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Int
             deriving (Eq,Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir 234

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x

data Example = MakeExample Int deriving Show

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43

newtype Geez = Geez (Int, String) deriving Show

instance TooMany Geez where
  tooMany (Geez (x, _)) = tooMany x

data Fruit = Peach
           | Plum
           | Apple
           | Blackberry
           deriving (Eq, Show, Ord)

data JamJars = Jam { fruit :: Fruit
                   , jars  :: Int }
             deriving (Eq, Show, Ord)

row1 = Jam Peach 23
row2 = Jam Plum 89
row3 = Jam Blackberry 3
row4 = Jam Apple 9
row5 = Jam Blackberry 23
row6 = Jam Peach 33
allJam = [row1, row2, row3, row4, row5, row6]

jarsOJam :: [JamJars] -> Int
jarsOJam = sum . map jars

mostRow :: [JamJars] -> JamJars
mostRow = foldr1 (\a b -> if jars a > jars b then a else b)

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

sortJams :: [JamJars] -> [JamJars]
sortJams = sortBy compareKind

sortJars :: [JamJars] -> [[JamJars]]
sortJars = groupBy (\a b -> fruit a == fruit b) . sortJams

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) = Node (mapTree f left) (f x) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = [x] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = inorder left ++ [x] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = postorder left ++ postorder right ++ [x]

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f x = foldr f x . inorder

conv :: String -> String -> Int -> String
conv [] _ _ = []
conv (' ':xs) ys n = ' ' : conv xs ys n
conv (_:xs) ys n = ys !! (findIndex n - 1) : conv xs ys (n + 1)
  where findIndex a =
          case a `mod` (length ys) of
            0 -> length ys
            b -> b

cipher :: Int -> Char -> Char
cipher _ ' ' = ' '
cipher n x
  | n > 0 && x == 'z' = cipher (n - 1) 'a'
  | n > 0 && x == 'Z' = cipher (n - 1) 'A'
  | n > 0 = cipher (n - 1) $ chr (ord x + 1)
  | n < 0 && x == 'a' = cipher (n + 1) 'z'
  | n < 0 && x == 'A' = cipher (n + 1) 'Z'
  | n < 0 = cipher (n + 1) $ chr (ord x - 1)
  | otherwise = x

cipherUser :: IO String
cipherUser = do
  putStr "enter text to cipher: "
  toCipher <- getLine
  putStr "enter key: "
  cipherWith <- getLine
  return $ vigCipher toCipher cipherWith

uncipherUser :: IO String
uncipherUser = do
  putStr "enter text to uncipher: "
  unCipher' <- getLine
  putStr "enter key: "
  uncipherWith <- getLine
  return $ unCipher unCipher' uncipherWith


vigCipher :: String -> String -> String
vigCipher xs ys =
  let convert = conv xs ys 1
      vigC [] _ = []
      vigC (a:as) (b:bs) = cipher (ord b - 97) a : vigC as bs
  in vigC xs convert

unCipher :: String -> String -> String
unCipher xs ys =
  let convert = conv xs ys 1
      vigC [] _ = []
      vigC (a:as) (b:bs) = cipher (negate $ ord b - 97) a : vigC as bs
  in vigC xs convert

isSubsequenceOf1 :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf1 xs ys = all (`elem` ys) xs

isSubsequenceOf2 xs ys = and . map (`elem` ys) $ xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords a@(x:_) = foldr (\a b -> (a, fstUp a) : b) [] $ words a
  where fstUp (y:ys) = toUpper y : ys

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph xs =
  capitalizeWord (takening xs) ++ " " ++
  capitalizeParagraph (drop (length (takening xs) + 1) xs)
  where takening a = takeWhile (/='.') a ++ "."

data DaPhone = DaPhone [Button]
data Button = Button Char String deriving Show

phone :: DaPhone
phone = DaPhone [Button '1' "1", Button '2' "abc2", Button '3' "def3"
                , Button '4' "ghi4", Button '5' "jkl5", Button '6' "mno6"
                , Button '7' "pqrs7", Button '8' "tuv8", Button '9' "wxyz9"
                , Button '*' "*", Button '0' " 0", Button '#' ",.#"]

type Digit = Char
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone (Button x y : xs)) a
  | a `elem` y = [(x, 1 + findDigit a y)]
  | toLower a `elem` y = [('*', 1), (x, 1 + findDigit (toLower a) y)]
  | otherwise = reverseTaps (DaPhone xs) a
  where findDigit m n= length $ takeWhile (/=m) n

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead ns [] = []
cellPhonesDead ns (x:xs) = reverseTaps ns x ++ cellPhonesDead ns xs

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps [] = 0
fingerTaps ((_,x):xs) = x + fingerTaps xs

fingerTaps' :: [(Digit, Presses)] -> Presses
fingerTaps' = foldr (\(_,a) b -> a + b) 0

count :: Eq a => Int -> [a] -> [Int]
count n xs
  | n < length xs = foldr (\a b -> if a == (xs !! n) then 1 + b else b) 0 xs : count (n + 1) xs 
  | otherwise = []

mostPopularLetter :: String -> Char
mostPopularLetter xs = fst . maxi . takeOut . sort . zip xs $ (count 0 xs)
  where takeOut = dropWhile (\(a, _) -> a == ' ' || a == '*' || a == '#')
        maxi = foldr1 (\s@(_,a) s2@(_,b) -> if a > b then s else s2)

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . map mostPopularLetter

coolestWord :: [String] -> String
coolestWord xs = snd . maximum . zip (count 0 (words . concat $ xs)) $ (words . concat $ xs)

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
