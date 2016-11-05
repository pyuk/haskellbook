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
   
