data Point a = Point a a
  deriving (Show)
data Shape = Circle (Point Float) Float |
             Rectangle (Point Float) (Point Float)
  deriving (Show)

surface :: Shape -> Float  
surface shape = case shape of
                  (Circle _ r)
                    -> pi * r ^ 2  
                  (Rectangle (Point x1 y1) (Point x2 y2))
                    -> (abs (x2 - x1)) * (abs (y2 - y1)) 
