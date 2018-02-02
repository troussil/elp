repli :: [a] -> Int -> [a]
repli lst c = case (lst,c) of
                ( (x:xs), c ) -> addElemInList x c (repli xs c)
                ( [], c ) -> []
             
addElemInList x c lst
  | c <= 0 = lst
  | c > 0 = addElemInList x (c-1) (x:lst)

