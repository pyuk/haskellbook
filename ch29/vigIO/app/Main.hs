module Main where

import VigCipher
import Lib
import System.Environment
import System.IO

vigIO :: String -> String -> String
vigIO arg key = vigCipher arg key

main1 :: IO ()
main1 = do
  arg <- putStr "enter message: " >> getLine
  key <- putStr "enter key: " >> getLine
  print $ vigIO arg key

main' :: IO ()
main' = vigIO <$>
        (putStr "enter message: " >> getLine) <*>
        (putStr "enter key: "     >> getLine) >>=
        print

main :: IO ()
main = do
  (key:mode:text) <- getArgs
  case mode of
    "-d" -> print $ vigCipher (mconcat text) key
    "-e" -> print $ unCipher (mconcat text) key
    []   -> putStrLn "please pass args"
