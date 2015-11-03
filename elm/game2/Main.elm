import Graphics.Element exposing (container, middle, Element)
import Graphics.Collage exposing (rect, collage, filled, move)
import Color exposing (rgb)
import Keyboard
import Time exposing (fps, inSeconds)

-- INIT

modelInit = Model (Position 20 20)

-- SIGNALS

main = Signal.map view model

model = Signal.foldp update modelInit input

delta =
  Signal.map inSeconds (fps 35)

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map Input
      (Signal.map .y Keyboard.arrows)

-- MODEL

type alias Position =
    { x : Float
    , y : Float
    }

type alias Input =
    { dir : Int}

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
update {dir} ({squarePosition} as model) =
    let newSquarePosition = updateSquare dir squarePosition
    in {model | squarePosition <- newSquarePosition}

updateSquare : Int -> Position -> Position
updateSquare dir ({x,y} as position) =
    { position |
        x <- x,
        y <- y + toFloat dir
    }

