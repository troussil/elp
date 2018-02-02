group :: (Eq a) => [a] -> [[a]]
group lst = case lst of
              [] -> []
              (x:xs) -> case res of
                          [] -> [[x]]
                          (lst:lsts)
                            | x == head lst -> (x:lst):lsts
                            | otherwise -> [x]:res
                where res = group xs
