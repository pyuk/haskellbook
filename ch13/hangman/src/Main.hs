-- src/Main.hs

module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- type WordList = [String]
newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 7

gameWords :: IO WordList
gameWords = do
  WordList aw <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in  l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = 
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle xs = Puzzle xs (fmap (const Nothing) xs) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) x = x `elem` s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ s) x = x `elem` s

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just a) = a

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_,True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (False,_) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)
    (True,_) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver puz@(Puzzle wordToGuess _ guessed) =
  if (wrongGuess puz) > 9 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
   else return ()

wrongGuess :: Puzzle -> Int
wrongGuess (Puzzle word filledIn guessed) =
  (length guessed) - (length . filter isJust $ filledIn)

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"
        

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (paliChecker line1 == reverse (paliChecker line1)) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!" >> exitSuccess

paliChecker :: String -> String
paliChecker xs = filter (`elem` "abcdefghijklmnopqrstuvwxyz") .
                 map toLower . concat . words $ xs

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)
mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                "Name was: " ++ show name ++
                "Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "enter name: "
  name <- getLine
  putStr "enter age: "
  age <- getLine  
  case mkPerson name (read age) of
    Right x -> putStrLn $ "Yay! Successfully got a person: " ++ show x
    Left x -> putStrLn $ "not a valid person: " ++ show x
  exitSuccess
