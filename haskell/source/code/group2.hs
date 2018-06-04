group :: (Eq a) => [a] -> [[a]]
group []     = []
group (x:xs) = (x:(filter (==x) xs))
                 :(group (filter (/=x) xs))
