myReverse :: [a] -> [a]
myReverse = foldl (\xs x -> x:xs) [] 
