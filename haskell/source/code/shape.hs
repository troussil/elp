data Point a = Point a a
  deriving (Show)
data Shape a = Circle (Point a) a |
             Rectangle (Point a) (Point a)
  deriving (Show)

surface :: Shape Float -> Float  
surface shape = case shape of
                  (Circle _ r)
                    -> pi * r ^ 2  
                  (Rectangle (Point x1 y1) (Point x2 y2))
                    -> (abs (x2 - x1)) * (abs (y2 - y1)) 
