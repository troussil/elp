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

encode : List comparable -> List (Int,comparable)
encode lst = case lst of
  [] -> []
  (x :: xs) -> case (encode xs) of 
    [] -> [(1,x)]
    ((nb,elem) :: ys) -> if x == elem
                         then (nb+1,elem) :: ys
                         else (1,x) :: (nb,elem) :: ys 

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

encodeHelper : comparable -> List (Int,comparable) -> List (Int,comparable)
encodeHelper x partialRes = case partialRes of 
    [] -> [(1,x)]
    ((nb,elem) :: ys) -> if x == elem
                         then (nb+1,elem) :: ys
                         else (1,x) :: (nb,elem) :: ys 

encode2 : List comparable -> List (Int,comparable)
encode2 lst = List.foldr encodeHelper [] lst

