myLen :: [a] -> Integer
myLen lst = case lst of
              [] -> 0
              (x:xs) -> 1 + myLen xs
