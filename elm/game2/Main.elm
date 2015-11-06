import Graphics.Element exposing (container, bottomLeft, Element)
import Graphics.Collage exposing (rect, collage, filled, move, Form)
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
        [drawSquare model.player
        ,drawSquare model.enemy
        ]

-- UPDATE

update : Input -> Model -> Model
update input ({player, enemy} as model) =
    let newPlayer = movePlayer input player |> hitToColorChange enemy
    in {model | player <- newPlayer}

movePlayer : Input -> Square -> Square
movePlayer input ({left,bottom} as square) =
    { square |
        left <- left + ((toFloat input.arrows.x) * speed),
        bottom <- bottom + ((toFloat input.arrows.y) * speed)
    }

hitToColorChange : Square -> Square -> Square
hitToColorChange ({left,bottom, width, height, color} as enemy) ({left,bottom, width, height, color} as player) =
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

-- OTHER
drawSquare : Square -> Form
drawSquare square =
    rect square.width square.height |>
        filled square.color |>
        move (square.left, square.bottom)