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
