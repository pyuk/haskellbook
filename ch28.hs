import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as BV

bumpIt :: (Int, Int) -> (Int, Int)
bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

m2 = m

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

s2 = s

main1 :: IO ()
main1 = defaultMain
  [ bench "put together maps" $
    nf (mappend m) m2
  , bench "put together sets" $
    nf (mappend s) s2
  ]

v :: V.Vector Int
v = V.fromList [1..1000]

bv :: BV.Vector Int
bv = BV.fromList [1..1000]

main2 :: IO ()
main2 = defaultMain
  [ bench "Plain Vector" $
    nf id v
  , bench "Boxed Vector" $
    nf id bv
  ]
   
newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL $ \_ -> []

singleton :: a -> DList a
singleton x = DL $ \_ -> [x]

toList :: DList a -> [a]
toList (DL f) = f []

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc (DL f) x = DL $ (++ [x]) . f

append :: DList a -> DList a -> DList a
append xs ys = foldr cons ys (toList xs)

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
  [ bench "concat list" $ whnf schlemiel 123456
  , bench "concat dlist" $ whnf constructDlist 123456
  ]
