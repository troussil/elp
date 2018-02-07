repeat x = x : repeat x

take n lst = case (n, lst) of
               (n, _) | n <= 0  ->  []
               (_, [])          ->  []
               (n, (x:xs))      ->  x : take (n-1) xs
