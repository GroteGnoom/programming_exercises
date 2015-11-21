import ElmTest exposing  (..)
import String exposing (..)
import Graphics.Element exposing (flow, down)
import Geometry exposing (Vector, Polygon, vecAdd, vecMul, intersectPolygonPolygon, distance, distanceSegmentVector)

minFloat = 0.000001

closeTo: Float -> Float -> Bool
closeTo f1 f2 =
    (f1 - f2 < minFloat) && (f2 - f1 < minFloat)

tests = 
    [ suite "Adding vectors"
        [ test "Distance between vector and itself should be zero" 
            (assert (closeTo 0 (distance {x=10,y=10} {x=10,y=10})))
        , test "Distance is symmetric between vectors" 
            (assert (closeTo (distance {x=10,y=10} {x=20,y=20}) (distance {x=20,y=20} {x=10,y=10})))
        ]
    , suite "distanceSegmentVector"
        [test "Distance between line segment and its start should be zero"
            (assert (closeTo 0 (distanceSegmentVector {a={x=10,y=10},b={x=20,y=10}} {x=10,y=10})))
        , test "Distance between line segment and its end should be zero"
            (assert (closeTo 0 (distanceSegmentVector {a={x=10,y=10},b={x=20,y=10}} {x=20,y=10})))
        , test "Distance between line segment and point in direction of segment should be distance to end point"
            (assert (closeTo 10 (distanceSegmentVector {a={x=10,y=10},b={x=20,y=10}} {x=30,y=10})))
        , test "Distance between line segment and point in line should be zero"
            (assert (closeTo 0 (distanceSegmentVector {a={x=10,y=10},b={x=20,y=10}} {x=15,y=10})))
        , test "Distance between line segment and point above line should be distance to line"
            (assert (closeTo 5 (distanceSegmentVector {a={x=10,y=10},b={x=20,y=10}} {x=15,y=15})))
        ]
    ]

main = 
    flow down 
    (List.map elementRunner tests)