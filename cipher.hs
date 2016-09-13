module Cihper where

import Data.Char

cipherHelper :: Int -> Char -> Char
cipherHelper 0 a = a
cipherHelper x a
  | x < 0 && a /= 'a' = cipherHelper (x + 1) (pred a)
  | x < 0 && a == 'a' = cipherHelper (x + 1) 'z'
  | a /= 'z' = cipherHelper (x - 1) (succ a)
  | otherwise = cipherHelper (x - 1) 'a'
  
cipherThing :: Int -> String -> String
cipherThing _ [] = []
cipherThing a (x:xs) = cipherHelper a x : cipherThing a xs

unCaesar :: Int -> String -> String
unCaesar _ [] = []
unCaesar a (x:xs) = cipherHelper (-a) x : unCaesar a xs