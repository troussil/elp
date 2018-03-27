data Tree a = Leaf a | Node (Tree a) (Tree a)

treeHeight :: Tree a -> Int
treeHeight tree = case tree of
                    (Leaf _) -> 1
                    (Node l r) -> 1 +
                      max (treeHeight l) (treeHeight r) 
