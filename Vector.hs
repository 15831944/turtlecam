module Vector
( Point(..)
, Vector(..)
, plus
, mult
) where

data Point = Point Double Double Double deriving (Show, Eq)

data Vector a = Vector a a a deriving (Show, Eq)

plus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x0 y0 z0) `plus` (Vector x1 y1 z1) = Vector (x0+x1) (y0+y1) (z0+z1)

mult :: (Num a) => Vector a -> a -> Vector a
(Vector x y z) `mult` a = Vector (x*a) (y*a) (z*a)
