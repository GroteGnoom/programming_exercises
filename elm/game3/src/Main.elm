import Graphics.Element exposing (container, bottomLeft, Element)
import Graphics.Collage exposing (polygon, collage, filled, Form)
import Color exposing (rgb, Color)
import Geometry exposing (Vector, Polygon, vecAdd, vecMul, intersectPolygonPolygon)
import Time exposing (fps, inSeconds)
import Keyboard

-- GLOBALS / INIT

modelInit : Model
modelInit =
    { player =
        {outline =
            [ {x = 20, y = 20}
            , {x = 20, y = 40}
            , {x = 40, y = 40}
            , {x = 40, y = 20}
            ]
        , color = rgb 100 100 100
        , draw = drawPolygon
        , move = polygonMove
        , speed = 3.0
        }
    , enemies =
        [
            {outline =
                [ {x = 120, y = 120}
                , {x = 120, y = 140}
                , {x = 140, y = 140}
                , {x = 140, y = 120}
                ]
            , color = rgb 200 0 0
            , draw = drawPolygon
            , move = polygonMove
            , speed = 3.0
            }
        ]
    }

-- SIGNALS

main : Signal Element
main = Signal.map view model

model : Signal Model
model = Signal.foldp update modelInit input

delta : Signal Float
delta =
  Signal.map inSeconds (fps 35)

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map Input Keyboard.arrows


-- MODEL


type alias Input =
    { arrows :
        { x : Int
        , y : Int
        }
    }

type alias Model =
    { player : Creature
    , enemies : List Creature
    }

type alias Creature =
    { outline : Polygon
    , color : Color
    , draw : (Polygon -> Color -> Form)
    , move : (Vector -> Polygon -> Polygon)
    , speed : Float
    }

-- VIEW
view : Model -> Element
view model = container 500 500 bottomLeft <|
    collage 500 500
        ((drawCreature model.player) :: (List.map drawCreature model.enemies))


-- UPDATE
update : Input -> Model -> Model
update input ({player, enemies} as model) =
    let movedPlayer = moveCreature (vecMul player.speed (inputToVector input)) player
    in  {model |
            player
                = if (List.any (intersect player) enemies)
                    then changeColor movedPlayer (rgb 100 100 100)
                    else changeColor movedPlayer (rgb 200 200 200)
        }

moveCreature : Vector -> Creature -> Creature
moveCreature  vec creature =
    {creature |
        outline = creature.move vec creature.outline
    }

intersect : Creature -> Creature -> Bool
intersect c1 c2 =
    intersectPolygonPolygon c1.outline c2.outline

changeColor : Creature -> Color -> Creature
changeColor creature color =
    {creature |
        color = color
    }

-- OTHER


inputToVector : Input -> Vector
inputToVector input =
    { x = (toFloat input.arrows.x)
    , y = (toFloat input.arrows.y)
    }

drawPolygon : Polygon -> Color -> Form
drawPolygon poly color =
    polygon (polyToLF poly) |> filled color

polygonMove : Vector -> Polygon -> Polygon
polygonMove vec poly  =
    List.map (vecAdd vec) poly

polyToLF : Polygon -> List (Float, Float)
polyToLF poly =
    List.map vectorToFF poly

vectorToFF : Vector -> (Float, Float)
vectorToFF vec =
    (vec.x, vec.y)

drawCreature : Creature -> Form
drawCreature creature =
 creature.draw creature.outline creature.color
