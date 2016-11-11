module VigCipher where

import Data.Char

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
