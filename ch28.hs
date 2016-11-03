import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

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

main :: IO ()
main = defaultMain
  [ bench "put together maps" $
    nf (mappend m) m2
  , bench "put together sets" $
    nf (mappend s) s2
  ]
