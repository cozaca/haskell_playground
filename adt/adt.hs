-- | ADT, create your own data, and print it on console

data Point = Point Float Float deriving Show
data Shape = Circle Point Float | Rectangle Point Point deriving Show 

surface :: Shape -> Float
surface (Circle _ r) = r * pi * 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs (x2-x1)) * (abs (y2 - y1))

-- | Impl a function that moves shapes from one point to another

move :: Shape -> Point -> Shape
move (Circle (Point x y) r) (Point a b) = Circle (Point (x+a) (y+b)) r