import Prelude hiding (length, sum, product, and, or, any, all)
--on cache les fonctions existantes pour les redéfinir

length :: [a] -> Int
length = foldr (\_ y -> y+1) 0
sum :: Num a => [a] -> a
sum = foldr (+) 0
product :: Num a => [a] -> a
product = foldr (*) 1
and :: [Bool] -> Bool
and = foldr (&&) True
or :: [Bool] -> Bool
or = foldr (||) False
any :: (a -> Bool) -> [a] -> Bool
any p = foldr (\x y -> (p x) || y) False
all :: (a -> Bool) -> [a] -> Bool
all p = foldr (\x y -> (p x) && y) True
