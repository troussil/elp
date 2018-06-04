repli :: [a] -> Int -> [a]
repli (x:xs) c = addElemInList x c (repli xs c)
repli [] c     = []

addElemInList x c lst
  | c <= 0 = lst
  | c > 0 = addElemInList x (c-1) (x:lst)
