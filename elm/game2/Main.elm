import Graphics.Element exposing (container, middle, Element)
import Graphics.Collage exposing (rect, collage, filled, move)
import Color exposing (rgb)
import Keyboard
import Time exposing (fps, inSeconds)

-- GLOBALS

speed : Float
speed = 3

-- INIT

modelInit : Model
modelInit = Model (Position 20 20)

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

type alias Position =
    { x : Float
    , y : Float
    }

type alias Input =
    { arrows : 
        {x : Int,
         y : Int
        }
    }

type alias Model =
    {squarePosition : Position
    }

-- VIEW
view : Model -> Element
view model = container 500 500 middle <|
    collage 500 500
        [rect 20 20 |>
        filled (rgb 100 100 100) |>
        move (model.squarePosition.x, model.squarePosition.y)
        ]

-- UPDATE

update : Input -> Model -> Model
update input ({squarePosition} as model) =
    let newSquarePosition = updateSquare input squarePosition
    in {model | squarePosition <- newSquarePosition}

updateSquare : Input -> Position -> Position
updateSquare input ({x,y} as position) =
    { position |
        x <- x + ((toFloat input.arrows.x) * speed),
        y <- y + ((toFloat input.arrows.y) * speed)
    }

