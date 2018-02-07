slice :: [a] -> Int -> Int -> [a]
slice lst i j = case (lst, i, j) of 
                  ([], _, _)  -> []
                  ((x:xs), i, k) 
                    | i > 1 -> slice xs (i-1) (k-1)
                    | k < 1 -> []
                    | otherwise -> x:(slice xs (i-1) (k-1))
