module Test exposing (..)

-- fonctions recursives

addElemInList : a -> Int -> List a -> List a
addElemInList x nb lst = 
  if nb > 0
  then x :: (addElemInList x (nb-1) lst)
  else lst

dupli : List a -> List a
dupli lst = case lst of
  [] -> []
  (x :: xs) -> x :: x :: (dupli xs)

compress : List comparable -> List comparable
compress lst = case lst of
  [] -> []
  (x :: xs) -> case xs of
    [] -> [x]
    (y :: ys) -> if x == y
                 then compress xs
                 else x :: (compress xs)

--- fonctions du package List

addElemInList2 : a -> Int -> List a -> List a
addElemInList2 x nb lst = (List.repeat nb x) ++ lst

dupli2 : List a -> List a
dupli2 lst = List.concatMap (\x -> List.repeat 2 x) lst

compressHelper : comparable -> (List comparable) -> (List comparable)
compressHelper x partialRes = case partialRes of
  [] -> [x]
  (y :: ys) -> if x == y
               then partialRes
               else x :: partialRes
                   
compress2 : List comparable -> List comparable
compress2 lst = List.foldr compressHelper [] lst


