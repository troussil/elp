encode :: (Eq a) => [a] -> [(Int, a)]
encode [] =  []
encode (x:xs) = case res of
                  [] -> [(1,x)]
                  (y:ys) ->
                    case y of
                      (nb, elem)
                        | x == elem -> (nb+1,x):(ys)
                        | otherwise -> (1,x):res
  where res = encode xs 

