import Prelude hiding (minimum)

minimum :: (Ord a) => [a] -> Maybe a
minimum [] = Nothing
minimum [x] = Just x
minimum (x:xs)
   | (Just x) <= maybeMin = Just x
   | otherwise = maybeMin
 where maybeMin = minimum xs
