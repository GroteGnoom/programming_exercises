import Graphics.Element exposing (container, bottomLeft, Element)
import Graphics.Collage exposing (rect, collage, filled, move)
import Color exposing (rgb, Color)
import Keyboard
import Time exposing (fps, inSeconds)

-- GLOBALS

speed : Float
speed = 3

-- INIT

modelInit : Model
modelInit = { player = (Square 0 0 20 20 (rgb 100 100 100))
            , enemy = (Square 100 100 20 20 (rgb 200 200 200))
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

type alias Square =
    { left : Float
    , bottom : Float
    , width : Float
    , height : Float
    , color : Color
    }

type alias Position =
    { x : Float
    , y : Float
    }

type alias Input =
    { arrows : 
        { x : Int
        , y : Int
        }
    }

type alias Model =
    { player : Square
    , enemy : Square
    }

-- VIEW
view : Model -> Element
view model = container 500 500 bottomLeft <|
    collage 500 500
        [rect model.player.width model.player.height |>
        filled model.player.color |>
        move (model.player.left, model.player.bottom)
        ,rect model.enemy.width model.enemy.height |>
        filled model.enemy.color |>
        move (model.enemy.left, model.enemy.bottom)
        ]

-- UPDATE

update : Input -> Model -> Model
update input ({player, enemy} as model) =
    let newPlayer1 = updateSquare1 input player
    in let newPlayer2 = updateSquare2 enemy newPlayer1
        in {model | player <- newPlayer2}

updateSquare1 : Input -> Square -> Square
updateSquare1 input ({left,bottom} as square) =
    { square |
        left <- left + ((toFloat input.arrows.x) * speed),
        bottom <- bottom + ((toFloat input.arrows.y) * speed)
    }

updateSquare2 : Square -> Square -> Square
updateSquare2 ({left,bottom, width, height, color} as enemy) ({left,bottom, width, height, color} as player) =
    if hitSquareSquare player enemy
        then
            { player |
                color <- rgb 255 0 0
            }
        else 
            { player |
                color <- rgb 100 100 100
            }

-- COLLISION DETECTION

hitSquareSquare : Square -> Square -> Bool
hitSquareSquare square1 square2 =
    square1.left < square2.left + square2.width &&
    square1.left + square1.width > square2.left &&
    square1.bottom < square2.bottom + square2.height &&
    square1.bottom + square1.height > square2.bottom
