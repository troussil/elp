quicksort :: (Ord a) => [a] -> [a]
quicksort lst = case lst of
                  [] -> []
                  (x:xs) -> quicksort [y | y <- xs, y < x]
                            ++ [x]
                            ++ quicksort [y | y <- xs, y >= x]
