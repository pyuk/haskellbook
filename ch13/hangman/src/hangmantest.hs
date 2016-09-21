module HangmanTest where

import Main
import Test.Hspec
import Test.QuickCheck

prop_puz :: Char -> Bool
prop_puz x = (fillInCharacter puz x) ==
             (if x `elem` "cat" then puz' else puz'')
  where puz = Puzzle "cat" [Nothing, Nothing, Nothing] []
        puz'
          | x == 'c' = Puzzle "cat" [Just x, Nothing, Nothing] [x]
          | x == 'a' = Puzzle "cat" [Nothing, Just x, Nothing] [x]
          | x == 't' = Puzzle "cat" [Nothing, Nothing, Just x] [x]
        puz'' = Puzzle "cat" [Nothing,Nothing,Nothing] [x]

prop_handle :: Char -> IO Bool
prop_handle x = fmap (==(if x `elem` "cat" then puz' else puz''))
                (handleGuess puz x)
  where puz = Puzzle "cat" [Nothing, Nothing, Nothing] []
        puz'
          | x == 'c' = Puzzle "cat" [Just x, Nothing, Nothing] [x]
          | x == 'a' = Puzzle "cat" [Nothing, Just x, Nothing] [x]
          | x == 't' = Puzzle "cat" [Nothing, Nothing, Just x] [x]
        puz'' = Puzzle "cat" [Nothing,Nothing,Nothing] [x]
