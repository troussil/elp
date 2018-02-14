data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten nl = case nl of
               (Elem x) -> [x]
               (List []) -> []
               (List (x:xs)) -> flatten x ++ flatten (List xs)

flatten' :: NestedList a -> [a]
flatten' nl = f nl []
  where f nl res = case (nl,res) of
                     (Elem x,ys) -> x:ys
                     (List l,ys) -> foldr f ys l
                     
