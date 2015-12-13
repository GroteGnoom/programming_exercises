import ElmTest.Test exposing  (..)
import ElmTest.Assertion exposing (..)
import ElmTest.Runner.Element exposing (..)
import String exposing (..)
import Graphics.Element exposing (flow, down)
import Geometry exposing (SquareRecord, CircleRecord, PolygonRecord, Shape, Segment,
                            hitSquareSquare, intersectCircleSquare, intersectSegmentCircle, 
                            inCircle, distance, distanceSegmentVector)

minFloat = 0.00001

vector0 =
    { x = 0
    , y = 0
    }

vectorx10y10 =
    { x = 10
    , y = 10
    }

vectorx20y20 =
    { x = 20
    , y = 20
    }

vectorx20y10 =
    { x = 20
    , y = 10
    }

vector4 =
    { x = 15
    , y = 15
    }

vector5 =
    { x = 15
    , y = 10
    }

vector6 =
    { x = 30
    , y = 10
    }

segment1 = 
    { a = vectorx10y10
    , b = vectorx20y10
    }

segment2 = 
    { a = vector0
    , b = vectorx20y10
    }

circle1 =
    { center = vector4
    , radius = 3
    }

circle2 =
    { center = vector5
    , radius = 3
    }

circle3 =
    { center = vector0
    , radius = 8
    }

circle4 =
    { center = vector0
    , radius = 7
    }

circle5 =
    { center = {x = 0, y = 10}
    , radius = 4
    }

circle6 =
    { center = {x = 0, y = 10}
    , radius = 6
    }

square1 =
    { left = -5
    , bottom = -5
    , width = 10
    , height = 10
    }

closeTo: Float -> Float -> Bool
closeTo f1 f2 =
    (f1 - f2 < minFloat) && (f2 - f1 < minFloat)

tests = 
    [ suite "Distance"
        [ test "Distance between vector and itself should be zero" 
            (assert (closeTo 0 (distance vectorx10y10 vectorx10y10)))
        , test "Distance is symmetric between vectors" 
            (assert (closeTo (distance vectorx10y10 vectorx20y20) (distance vectorx20y20 vectorx10y10)))
        ]
    , suite "distanceSegmentVector"
        [test "Distance between line segment and its start should be zero"
            (assert (closeTo 0 (distanceSegmentVector segment1 vectorx10y10)))
        , test "Distance between line segment and its end should be zero"
            (assert (closeTo 0 (distanceSegmentVector segment1 vectorx20y10)))
        , test "Distance between line segment and point in direction of segment should be distance to end point"
            (assert (closeTo 10 (distanceSegmentVector segment1 vector6)))
        , test "Distance between line segment and point in line should be zero"
            (assert (closeTo 0 (distanceSegmentVector segment1 vector5)))
        , test "Distance between line segment and point above line should be distance to line"
            (assert (closeTo 5 (distanceSegmentVector segment1 vector4)))
        ]
    , suite "Intersect linesegment circle"
        [ test "Line segment under circle should not intersect it" 
            (assert (not (intersectSegmentCircle segment1 circle1)))
        , test "Line segment through circle should intersect it" 
            (assert (intersectSegmentCircle segment1 circle2))
        , test "Line segment inside circle should not intersect it" 
            (assert (not (intersectSegmentCircle segment1 circle4)))
        ]
    , suite "Intersect circle square"
        [ test "Circle in square should not intersect it" 
            (assert (not (intersectCircleSquare circle1 square1)))
        , test "Circle around square should not intersect it" 
            (assert (not (intersectCircleSquare circle3 square1)))
        , test "Circle and square with matching centers and radius between half side and half diagonal should intersect" 
            (assert (intersectCircleSquare circle4 square1))
        , test "Circle above square should not intersect it" 
            (assert (not (intersectCircleSquare circle5 square1)))
        , test "Larger circle above square should intersect it" 
            (assert (intersectCircleSquare circle6 square1))
        ]
    ]

main = 
    flow down 
    (List.map runDisplay tests)



