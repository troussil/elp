group :: (Eq a) => [a] -> [[a]]
group lst = case lst of
              [] -> []
              (x:xs) -> (x:(filter (==x) xs)):(group (filter (/=x) xs))
