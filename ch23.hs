import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
            let (die, nextGen) = randomR (1,6) gen
            in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= n = count
          | otherwise =
            let (die, nextGen) = randomR (1,6) gen
            in go (sum + die) (count + 1) nextGen

data Die = DieOne
         | DieTwo
         | DieThree
         | DieFour
         | DieFive
         | DieSix
         deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use this tactic _extremely_ sparingly.
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] g
  where go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
        go sum count log gen
          | sum >= n = (count, log)
          | otherwise =
            let (die, nextGen) = randomR (1,6) gen
            in go (sum + die) (count + 1) (log ++ [intToDie die]) nextGen

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s -> ((f . fst) (g s), s)

instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a, s)
  (Moi f) <*> (Moi g) = Moi $ \s -> ((fst . f $ s) (fst . g $ s), s)

instance Monad (Moi s) where
  return = pure
  (Moi f) >>= g = Moi $ \s -> (runMoi . g . fst . f) s (snd . f $ s)

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Fizz"
           | n `mod` 3 == 0 = "Buzz"
           | otherwise = show n

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo n m = fmap fizzBuzz [m, (m - 1)..n]

get' :: Moi s s
get' = Moi $ \s -> (s, s)

put' :: s -> Moi s ()
put' s = Moi $ \s' -> ((), s)

exec' :: Moi s a -> s -> s
exec' (Moi sa) = snd . sa

eval' :: Moi s a -> s -> a
eval' (Moi sa) = fst . sa

modify'' :: (s -> s) -> Moi s ()
modify'' f = Moi $ \s -> ((), f s)
