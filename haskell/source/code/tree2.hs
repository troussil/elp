data Tree a = Leaf a | Node (Tree a) (Tree a)

flatten :: Tree a -> [a] 
flatten tree = f tree []
  where f t lst = case (t,lst) of
                    (Leaf x, lst) -> x:lst
                    (Node l r, lst) -> f l (f r lst)
