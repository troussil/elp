class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)
