import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative
import Data.Ratio ((%))
 
stop :: Parser a
stop = unexpected "stop"

one = eof >> char '1'
one' = one >> stop
oneTwo = char '1' >> eof >> char '2'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

exone = string "1" >> stop
extwo = string "12" >> stop
exthree = string "123" >> stop
exthree' = charString'' "123" >> stop

charString [] = stop
charString (p:ps) = char p >> charString ps

charString' :: String -> Parser String
charString' = foldr (\a b -> char a >> b) stop

charString'' :: String -> Parser String
charString'' = foldr ((>>) . char) stop

pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

parseDecimal :: Parser String
parseDecimal = do
  dec <- decimal
  char '.'
  af <- decimal
  return (show dec ++ "." ++ show af)

parseDecOrFrac :: Parser (Either Rational String)
parseDecOrFrac = (Left <$> try parseFraction) <|> (Right <$> parseDecimal)

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)

parseVer :: Parser (Major, Minor, Patch)
parseVer = do
  major <- decimal
  _ <- char '.'
  minor <- decimal
  _ <- char '.'
  patch <- decimal
  return (major, minor, patch)

parsePre :: Parser Release
parsePre = do
  _ <- char '-'
  pre <- sepBy1 ((NOSI <$> integer) <|> (NOSS <$> some letter)) (char '.')
  return pre
  
parseMeta :: Parser Metadata
parseMeta = do
  _ <- char '+'
  meta <- sepBy1 ((NOSS <$> some letter) <|> (NOSI <$> integer)) (char '.')
  return meta
  
parseSemVer :: Parser SemVer
parseSemVer = do
  (maj, min, pat) <- parseVer
  pre <- parsePre <|> pure []
  meta <- parseMeta <|> pure []
  return $ SemVer maj min pat pre meta

instance Ord SemVer where
  compare (SemVer x y z _ _) (SemVer x' y' z' _ _) =
    case compare x x' of
      EQ -> case compare y y' of
        EQ -> case compare z z' of
          EQ -> EQ
          LT -> LT
          GT -> GT
        LT -> LT
        GT -> GT
      LT -> LT
      GT -> GT

parseDigit :: Parser Char
parseDigit = char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|>
             char '6' <|> char '7' <|> char '8' <|> char '9' <|> char '0'

base10Integer :: Parser Integer
base10Integer = fmap read $ some parseDigit

base10Integer' :: Parser Integer
base10Integer' = do
  _ <- char '-'
  bas <- base10Integer
  return $ negate bas

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parseParenthe :: Parser a -> Parser a
parseParenthe p = char '(' *> p <* char ')'

parsePhone1 :: Parser (NumberingPlanArea, Exchange, LineNumber)
parsePhone1 = do
  string "1-" <|> return ""
  a <- parseParenthe integer <|> integer
  char '-' <|> char ' '
  b <- integer
  char '-'
  c <- integer
  return (fromIntegral a,fromIntegral b, fromIntegral c)

parseP :: String -> (Int, Int, Int)
parseP = (,,) <$> read . take 3 <*> read . take 3 . drop 3 <*> read . drop 6

parsePhone2 :: Parser (NumberingPlanArea, Exchange, LineNumber)
parsePhone2 = do
  a <- some digit
  b <- return $ parseP a
  return b

parsePhone :: Parser PhoneNumber
parsePhone = do
  (a, b, c) <- try parsePhone1 <|> parsePhone2
  return $ PhoneNumber a b c

type Hour = Integer
type Min = Integer
type Event = String

data Activity = Activity Hour Min Event
  deriving (Show, Eq)

parseAct :: Parser Activity
parseAct = do
  hour <- integer
  char ':'
  min <- integer
  event <- some (noneOf "--")
  try parseComment <|> return []
  skipMany (oneOf "\n")
  return $ Activity hour min event

parseComment :: Parser String
parseComment = do
  char '-' >> char '-' >> some (noneOf "\n")

parseActivity :: Parser [Activity]
parseActivity = do
  (char '#' >> char ' ' >> some (noneOf "--")) <|> return []
  try parseComment <|> return []
  skipMany (oneOf "\n")
  a <- some parseAct
  skipMany (oneOf "\n")
  return a
