
data Tree a = Empty | Node a (Tree a) (Tree a)

flatten :: Tree a -> [a] 
flatten tree = f tree []
  where f t lst = case (t,lst) of
                    (Empty, lst) -> lst
                    (Node v l r, lst) -> f l (v:(f r lst))
