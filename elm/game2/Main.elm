import Graphics.Element exposing (container, bottomLeft, Element)
import Graphics.Collage exposing (rect, collage, filled, move, Form)
import Color exposing (rgb, Color)
import Keyboard
import Time exposing (fps, inSeconds)

import Geometry exposing (SquareRecord, CircleRecord, PolygonRecord, Shape, Segment,
                            hitSquareSquare, intersectCircleSquare)


-- GLOBALS

speed : Float
speed = 3

-- INIT

modelInit : Model
modelInit = 
    { player = 
        { outline = Geometry.Square
            { left = 100
            , bottom = 100
            , width = 50
            , height = 50
            }
        , color = rgb 100 100 100
        }
    , enemies = 
        [
            { outline = Geometry.Circle
                { center = 
                    { x = 100
                    , y = 100
                    }
                , radius = 30
                }
            , color = rgb 200 200 200
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

type alias Creature =
    { outline : Shape
    , color : Color
    }

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

-- VIEW
view : Model -> Element
view model = container 500 500 bottomLeft <|
    collage 500 500
        ((draw model.player) :: (List.map draw model.enemies))
        
        

-- UPDATE

update : Input -> Model -> Model
update input ({player, enemies} as model) =
    case List.head enemies of
        Just enemy ->
            let newPlayer = movePlayer input player |> hitToColorChange enemy
            in {model | player <- newPlayer}
        Nothing ->
            let newPlayer = movePlayer input player
            in {model | player <- newPlayer}

movePlayer : Input -> Creature -> Creature
movePlayer input player =
    let playerOutline = player.outline
    in case player.outline of
        Geometry.Square square ->
            { player |
                outline <- 
                    Geometry.Square 
                    { square 
                        | left <- square.left + ((toFloat input.arrows.x) * speed)
                        , bottom <- square.bottom + ((toFloat input.arrows.y) * speed)
                    }
            }
    
hitToColorChange : Creature -> Creature -> Creature
hitToColorChange creature1 creature2 =
    case creature1.outline of
        Geometry.Square square1 ->
            case creature2.outline of
                Geometry.Square square2 ->
                    if hitSquareSquare square1 square2
                        then
                            colorChange creature2 (rgb 255 0 0)
                        else 
                            colorChange creature2 (rgb 100 100 100)
        Geometry.Circle circle1 ->
            case creature2.outline of
                Geometry.Square square2 ->
                    if intersectCircleSquare circle1 square2
                        then
                            colorChange creature2 (rgb 255 0 0)
                        else 
                            colorChange creature2 (rgb 100 100 100)

colorChange : Creature -> Color -> Creature
colorChange {outline, color} newColor =
    { outline = outline
    , color = newColor
    }

-- OTHER
draw : Creature -> Form
draw {outline, color} =
    case outline of
        Geometry.Square square ->
            rect square.width square.height |>
                filled color |>
                move (square.left + square.width/2, square.bottom + square.height/2)
        Geometry.Circle circle ->
            Graphics.Collage.circle circle.radius |>
                filled color |>
                move (circle.center.x, circle.center.y)

