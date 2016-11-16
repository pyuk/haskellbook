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

listTest :: [String] -> String
listTest [] = mempty
listTest [x] = x
listTest xs = mconcat xs

main2 :: IO ()
main2 = do
  (key:mode:text) <- getArgs
  contents <- readFile (listTest text)
  case mode of
    "-d" -> hPutStrLn stdout $ unCipher contents key
    "-e" -> hPutStrLn stdout $ vigCipher contents key
    otherwise   -> putStrLn "please pass -d or -e"

main :: IO ()
main = do
  key <- putStr "enter key: " >> getLine
  mode <- putStr "enter mode: " >> hGetChar stdin >> hGetChar stdin
  handle <- getLine >>=
    \x -> openFile x ReadWriteMode
  hBool <- hWaitForInput handle 3
  if hBool then do 
    contents <- hGetContents handle
    case mode of
      'e' -> hPutStr stdout $ vigCipher contents key
      'd' -> hPutStr handle $ unCipher contents key
      otherwise -> putStrLn "please pass -e or -d"
    else hPutStr stderr "too slow"
