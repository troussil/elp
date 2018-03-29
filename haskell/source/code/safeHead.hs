safeHead :: [a] -> Maybe a
safeHead lst = case lst of 
  [] -> Nothing
  (x:xs) -> Just x
