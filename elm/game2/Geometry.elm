module Geometry where

type alias SquareRecord =
    { left : Float
    , bottom : Float
    , width : Float
    , height : Float
    }

type alias CircleRecord =
    { center : Vector
    , radius : Float
    }

type alias PolygonRecord =
    { vectorList : List Vector
    }

type Shape
    = Square SquareRecord
    | Circle CircleRecord
    | Polygon PolygonRecord


type alias Vector =
    { x : Float
    , y : Float
    }

type alias Segment =
    { a : Vector
    , b : Vector
    }

    -- COLLISION DETECTION
hitSquareSquare : SquareRecord -> SquareRecord -> Bool
hitSquareSquare square1 square2 =
    square1.left < square2.left + square2.width &&
    square1.left + square1.width > square2.left &&
    square1.bottom < square2.bottom + square2.height &&
    square1.bottom + square1.height > square2.bottom


intersectCircleSquare : CircleRecord -> SquareRecord -> Bool
intersectCircleSquare circle square =
    intersectSegmentCircle 
        { a = (Vector square.left (square.bottom + square.height))
        , b = (Vector (square.left + square.width) (square.bottom + square.height))} circle ||
    intersectSegmentCircle 
        { a = (Vector (square.left + square.width)) (square.bottom + square.height)
        , b = (Vector (square.left + square.width) square.bottom)} circle ||
    intersectSegmentCircle 
        { a = (Vector (square.left + square.width) square.bottom)
        , b = (Vector square.left square.bottom)} circle ||
    intersectSegmentCircle 
        { a = (Vector square.left square.bottom)
        , b = (Vector square.left (square.bottom + square.height))} circle 


intersectSegmentCircle : Segment -> CircleRecord -> Bool
intersectSegmentCircle segment circle = 
    ((distanceSegmentVector segment circle.center) < circle.radius) && 
        ((distance segment.a circle.center) > circle.radius ||
        (distance segment.b circle.center) > circle.radius)

inCircle : Vector -> CircleRecord -> Bool
inCircle vector circle = 
    distance vector circle.center < circle.radius

distance : Vector -> Vector -> Float
distance p1 p2 =
    sqrt ((p1.x - p2.x)^2 + (p1.y - p2.y)^2)

distanceSegmentVector : Segment -> Vector -> Float
distanceSegmentVector segment vector =
    let l = sLength segment
    in 
        let t = vDot (vMin vector segment.a) (vMin segment.b segment.a) / l^2
        in 
            if (t < 0) then distance vector segment.a else
                if (t > 1.0) then distance vector segment.b else
                    distance vector (vPlus segment.a (vTimes t (vMin segment.b segment.a)))

vPlus : Vector -> Vector -> Vector
vPlus vector1 vector2 =
    { x = vector1.x + vector2.x
    , y = vector1.y + vector2.y}

vMin : Vector -> Vector -> Vector
vMin vector1 vector2 =
    { x = vector1.x - vector2.x
    , y = vector1.y - vector2.y}

vTimes : Float -> Vector -> Vector
vTimes float vector =
    { x = vector.x * float
    , y = vector.y * float}

vDot : Vector -> Vector -> Float
vDot vector1 vector2 =
    (vector1.x * vector2.x) +
    (vector1.y * vector2.y)

vLength : Vector -> Float
vLength vector =
    sqrt (vDot vector vector)

segmentToVector : Segment -> Vector
segmentToVector segment =
    vMin segment.b segment.a

sLength : Segment -> Float
sLength segment =
    vLength (segmentToVector segment)

