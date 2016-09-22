import Data.Monoid hiding ((<>))
import Test.QuickCheck
import Data.Semigroup

--monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
--monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

--monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
--monoidLeftIdentity a = (mempty <> a) == a
--
--monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
--monoidRightIdentity a = (a <> mempty) == a

data Optional a = Nada | Only a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1,return $ First' Nada)
              ,(1,return $ First' (Only a))]

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) Nada = Only x
  mappend Nada (Only x) = Only x
  mappend (Only x) (Only y) = Only (x `mappend` y)

newtype First' a = First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) x = x
  mappend x y = x

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

--main :: IO ()
--main = do
--  quickCheck (monoidAssoc :: FirstMappend)
--  quickCheck (monoidLeftIdentity :: FstId)
--  quickCheck (monoidRightIdentity :: FstId)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
