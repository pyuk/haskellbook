module Main where

import VigCipher
import Lib

vigIO :: String -> String -> String
vigIO arg key = vigCipher arg key

main :: IO ()
main = do
  arg <- putStr "enter message: " >> getLine
  key <- putStr "enter key: " >> getLine
  print $ vigIO arg key

main' :: IO ()
main' = vigIO <$>
        (putStr "enter message: " >> getLine) <*>
        (putStr "enter key: "     >> getLine) >>=
        print
