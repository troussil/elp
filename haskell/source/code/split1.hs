split lst n = case (lst, n) of
  ((x:xs), n)
    | n > 0 -> let (f, l) = split xs (n-1) in (x:f, l)
  (xs, n) -> ([], xs)
