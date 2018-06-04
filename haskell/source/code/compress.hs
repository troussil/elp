compress :: (Eq a) => [a] -> [a]
compress []  = []
compress [x] = [x]
compress (x:y:xs)
  | x == y    = compress (y:xs)
  | otherwise = x:(compress (y:xs))
