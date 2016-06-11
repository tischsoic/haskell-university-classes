import Data.Function
import Prelude

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldl (+) 0


product' :: (Foldable t, Num a) => t a -> a
product' = foldl (*) 1


flip' :: (a -> b -> c) -> b -> a -> c
flip' f a b = f b a


reverse' :: (Foldable t) => t a -> [a]
reverse' = foldl (flip' (:)) []


and' :: (Foldable t) => t Bool -> Bool
and' = foldl (&&) True


or' :: (Foldable t) => t Bool -> Bool
or' = foldl (||) True


head' :: (Foldable t) => t a -> a
head' = foldr (\el _ -> el) undefined


last' :: (Foldable t) => t a -> a
last' = foldl (\_ el -> el) undefined


map' :: (Foldable t) => (a -> b) -> t a -> [b]
map' f = foldl (\acc el -> acc ++ [f el]) []

map'' :: (Foldable t) => (a -> b) -> t a -> [b]
map'' f = foldr (\el acc -> f el : acc) []


filter' :: (Foldable t) => (a -> Bool) -> t a -> [a]
filter' f = foldl (
            \acc el ->
                if f el
                then acc ++ [el]
                else acc
            ) []

filter'' :: (Foldable t) => (a -> Bool) -> t a -> [a]
filter'' f = foldr (
            \el acc ->
                if f el
                then el : acc
                else acc
            ) []
