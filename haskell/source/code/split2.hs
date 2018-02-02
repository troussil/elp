split lst n = case (lst, n) of
  ([], n) -> ([], [])
  ((x:xs), n)
    | n > 0 -> (x:ys, zs)
    | otherwise -> ([], x:xs)
    where (ys,zs) = split xs (n-1)
