module Main where

import VigCipher
import Lib

vigIO :: String -> String -> IO ()
vigIO arg key = print $ vigCipher arg key

main :: IO ()
main = do
  arg <- getLine
  key <- getLine
  vigIO arg key
