addElemInList :: a -> Int -> [a] -> [a]
addElemInList x c lst
  | c <= 0 = lst
  | c > 0 = addElemInList x (c-1) (x:lst)
