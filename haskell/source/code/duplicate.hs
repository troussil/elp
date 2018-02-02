dupli :: [a] -> [a]
dupli lst = case lst of
              (x:xs) -> x:x:(dupli xs)
              [] -> []
