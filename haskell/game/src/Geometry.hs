module Geometry where

data Vector = Vector { x :: Float
                     , y :: Float
                     } deriving (Eq, Show)

data Segment = Segment { start :: Vector
                       , end :: Vector
                       } deriving (Eq, Show)

class WithLength a where
  len :: a -> Float

instance WithLength Vector where
  len vec = sqrt ((x vec ** 2) + (y vec ** 2))

instance WithLength Segment where
  len seg = len (end seg - start seg)

instance Num Vector where
   Vector a b + Vector c d=  Vector (a + b) (c + d)
   Vector a b * Vector c d = Vector (a*c) (b*d)
   Vector a b - Vector c d = Vector (a-c) (b-d)
   abs (Vector a b) = Vector (abs a) (abs b)
   signum (Vector a b) = Vector (signum a) (signum b)
   fromInteger i = Vector (fromInteger i) (fromInteger i)

main :: IO ()
main = print (3 * (Vector 3 3))
