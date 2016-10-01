import Data.Foldable
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\a b -> if a == x then True || b else False || b) False

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' x = getAny . foldMap (Any . (==x))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = Just . foldr1 (\a b -> if a < b then a else b)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = Just . foldr1 (\a b -> if a > b then a else b)

null' :: Foldable t => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: Foldable t => t a -> Int
length' = foldr (\a b -> 1 + b) 0 

length'' :: Foldable t => t a -> Int
length'' = getSum . foldMap (\_ -> Sum 1)

toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []

toList'' :: Foldable t => t a -> [a]
toList'' = foldMap (\a -> [a])

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

fold'' :: (Foldable t, Monoid m) => t m -> m
fold'' = foldr (\a b -> a `mappend` b) mempty

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = fold . foldr (\a b -> f a : b) []

data Constant a b = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two x y) = f y

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = (f y) `mappend` (f z)

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = (f b) `mappend` (f c) `mappend` (f d)
  foldr f x (Four' a b c d) = b `f` (c `f` (d `f` x))

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)
