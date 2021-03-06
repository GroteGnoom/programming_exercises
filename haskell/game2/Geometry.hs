{-# LANGUAGE MultiParamTypeClasses #-}
module Geometry where
import qualified Data.Maybe

data Vec = Vec { x :: Float
                     , y :: Float
                     } deriving (Eq, Show)

data Seg = Seg { start :: Vec
                       , end :: Vec
                       } deriving (Eq, Show)

data Poly = Poly [Vec]

class WithLength a where
  len :: a -> Float

instance WithLength Vec where
  len vec = sqrt ((x vec ** 2) + (y vec ** 2))

instance WithLength Seg where
  len seg = len (vSub (end seg) (start seg))

vAdd :: Vec -> Vec -> Vec
vAdd v1 v2 = Vec (x v1 + x v2) (y v1 + y v2)

vSub :: Vec -> Vec -> Vec
vSub v1 v2 = Vec (x v1 - x v2) (y v1 - y v2)

vCross :: Vec -> Vec -> Float
vCross v1 v2 = (x v1 * y v2) - (y v1 - x v2)

vMul :: Vec -> Float -> Vec
vMul (Vec a b) mul = Vec (a*mul) (b*mul)

vDiv :: Vec -> Float -> Maybe Vec
vDiv (Vec a b) _div =
  case _div of
    0 -> Nothing
    _ -> Just (Vec (a*_div) (b*_div))

class InterSectable a b where
  intersect :: a -> b -> Bool

instance InterSectable Seg Seg where
  intersect = intersectSegSeg

instance InterSectable Poly Poly where
  intersect = intersectPolyPoly

class Movable a where
  move :: a -> Vec -> a

instance Movable Vec where
  move = vAdd

instance Movable Poly where
  move (Poly poly) vec = Poly (map (vAdd vec) poly)

intersectSegSeg :: Seg -> Seg -> Bool
intersectSegSeg s1 s2 =
  let p = start s1
      r = end s1 `vSub` start s1
      q = start s2
      s = end s2 `vSub` start s2
      in
      let t = ((q `vSub` p) `vCross` s) / (r `vCross` s)
          u = ((q `vSub` p) `vCross` r) / (r `vCross` s)
          in
              t < 1 && u < 1

intersectPolyPoly :: Poly -> Poly -> Bool
intersectPolyPoly poly1 poly2 =
  let segmentCombi = cartProd (polyToSegs poly1) (polyToSegs poly2)
    in any (uncurry intersect) segmentCombi

polyToSegs :: Poly -> [Seg]
polyToSegs (Poly vecList) =
  let rotPoly = listRotate 1 vecList
      in zipWith Seg vecList rotPoly

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(xi,yi) | xi <- xs, yi <- ys]

listRotate :: Int -> [a] -> [a]
listRotate _ [] = []
listRotate n xs = zipWith const (drop n (cycle xs)) xs

vAvg :: [Vec] -> Maybe Vec
vAvg vecList =
  vSum vecList `vDiv` fromIntegral (length vecList)

vSum :: [Vec] -> Vec
vSum =
  foldl vAdd (Vec 0 0)

pCenter :: Poly -> Vec
pCenter (Poly vecList) =
  let result = vAvg vecList
  in Data.Maybe.fromMaybe
    (error "Trying to find the center of a polygon with no points")
    result
