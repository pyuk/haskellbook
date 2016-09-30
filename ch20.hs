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
null' = getAny . (Any . (elem'' []) . toList) 
