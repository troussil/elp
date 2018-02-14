class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  toList x = [x]

instance Listable Bool where
  toList True  = [1]
  toList False = [0]

instance Listable [Int] where
  toList = id

instance (Listable a, Listable b) => Listable (a,b) where
  toList (x,y) = toList x ++ toList y
