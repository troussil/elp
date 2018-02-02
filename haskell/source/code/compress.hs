compress :: (Eq a) => [a] -> [a]
compress lst = case lst of
                 (x:y:xs)
                   | x == y -> compress (y:xs)
                   | otherwise -> x:(compress (y:xs))
                 [x] -> [x]
                 [] -> []
