module Geometry where

import General exposing (zip, allCombinations, listRotate)

-- TYPES

type alias Vector =
    { x : Float
    , y : Float
    }

type alias Polygon =
    List Vector

type alias Segment =
    { a : Vector
    , b : Vector
    }
-- VECTORS

vecAdd : Vector -> Vector -> Vector
vecAdd vector1 vector2 =
    { x = vector1.x + vector2.x
    , y = vector1.y + vector2.y}

vecSub : Vector -> Vector -> Vector
vecSub vector1 vector2 =
    { x = vector1.x - vector2.x
    , y = vector1.y - vector2.y}

vecMul : Float -> Vector -> Vector
vecMul float vector =
    { x = vector.x * float
    , y = vector.y * float}

vecCross : Vector -> Vector -> Float
vecCross v w =
    v.x * w.y - v.y * w.x

vecDot : Vector -> Vector -> Float
vecDot vector1 vector2 =
    (vector1.x * vector2.x) +
    (vector1.y * vector2.y)

vecLength : Vector -> Float
vecLength vector =
    sqrt (vecDot vector vector)

distance : Vector -> Vector -> Float
distance p1 p2 =
    sqrt ((p1.x - p2.x)^2 + (p1.y - p2.y)^2)

-- POLYGONS

polyToSegments : Polygon -> List Segment
polyToSegments poly =
    let rotPoly = (listRotate poly)
        in List.map vectorsToSegment (zip poly rotPoly)

polyClose : Polygon -> Polygon
polyClose poly =
    List.append (poly) (List.take 1 poly)

-- INTERSECTIONS

intersectPolygonPolygon: Polygon -> Polygon -> Bool
intersectPolygonPolygon poly1 poly2 =
    let segmentCombi = allCombinations (polyToSegments poly1) (polyToSegments poly2)
        intersect (a,b) = intersectSegmentSegment a b
        in List.any intersect segmentCombi

intersectSegmentSegment: Segment -> Segment -> Bool
intersectSegmentSegment s1 s2 =
    let p = s1.a
        r = s1.b `vecSub` s1.a
        q = s2.a
        s = s2.b `vecSub` s2.a
        in 
        let t = ((q `vecSub` p) `vecCross` s) / (r `vecCross` s)
            u = ((q `vecSub` p) `vecCross` r) / (r `vecCross` s)
            in
                if ((t < 1) && (u < 1)) 
                    then True
                    else False

-- OTHER
segmentToVector : Segment -> Vector
segmentToVector segment =
    vecSub segment.b segment.a

segmentLength : Segment -> Float
segmentLength segment =
    vecLength (segmentToVector segment)

distanceSegmentVector : Segment -> Vector -> Float
distanceSegmentVector segment vector =
    let l = segmentLength segment
    in 
        let t = vecDot (vecSub vector segment.a) (vecSub segment.b segment.a) / l^2
        in 
            if (t < 0) then distance vector segment.a else
                if (t > 1.0) then distance vector segment.b else
                    distance vector (vecAdd segment.a (vecMul t (vecSub segment.b segment.a)))

vectorsToSegment : (Vector, Vector) -> Segment
vectorsToSegment (vec1, vec2) =
    { a = vec1
    , b = vec2
    }

