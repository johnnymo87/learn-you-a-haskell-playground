module Shapes
( Point(..)
, Shape(..)
, area
, nudge
) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
    deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudgePoint :: Point -> Float -> Float -> Point
nudgePoint (Point x y) nx ny = Point (x + nx) (y + ny)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle p r) nx ny = Circle (nudgePoint p nx ny) r
nudge (Rectangle p1 p2) nx ny = Rectangle (nudgePoint p1 nx ny) (nudgePoint p2 nx ny)
