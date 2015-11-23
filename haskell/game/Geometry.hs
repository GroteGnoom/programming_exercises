module Geometry where

data Vector = Vector { x :: Float
                     , y :: Float
                     } deriving (Show)

data Shape = Circle { center :: Vector
                    , radius :: Float
                    } deriving (Show)

length :: Vector -> Float
length vec = sqrt ((x vec ** 2) + (y vec ** 2))
