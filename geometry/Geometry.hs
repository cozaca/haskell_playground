-- | Making your own haskell modules

module Geometry 
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume ) where

sphereVolume :: Float -> Float
sphereVolume r = (4.0 / 3.0) * pi * (r^3)

sphereArea :: Float -> Float
sphereArea r = 4 * pi * (r^2)

cubeVolume :: Float -> Float
cubeVolume s = cuboidVolume s s s

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

cubeArea :: Float -> Float
cubeArea s = cuboidArea s s s

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea b c * 2