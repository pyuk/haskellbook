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
  deriving (Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata

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
  pre <- some $ (NOSS <$> some letter) <|> (NOSI <$> integer)
  skipMany (oneOf ".")
  skipMany (oneOf "\n")
  return pre
  
parseMeta :: Parser Metadata
parseMeta = do
  meta <- some $ (NOSS <$> some letter) <|> (NOSI <$> integer)
  skipMany (oneOf ".")
  return meta
  
parseSemVer :: Parser SemVer
parseSemVer = do
  (maj, min, pat) <- parseVer
  _ <- char '-'
  pre <- parsePre
  _ <- char '+'
  meta <- parseMeta
  return $ SemVer maj min pat pre meta
