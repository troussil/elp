myReverse xs = case xs of
                 [] -> []
                 (x:xs) -> myReverse xs ++ [x]
