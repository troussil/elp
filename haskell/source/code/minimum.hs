import Prelude hiding (minimum)

minimum :: (Ord a) => [a] -> Maybe a
minimum lst = case lst of
  [] -> Nothing
  l -> Just (minimum' l)
    where minimum' l = case l of
            [x] -> x
            (x:xs) -> min x (minimum' xs)
