data Tree a = Leaf a | Node (Tree a) (Tree a)

flatten :: Tree a -> [a]
flatten tree = case tree of
                 (Leaf x) -> [x]
                 (Node left right) -> flatten left ++ flatten right
