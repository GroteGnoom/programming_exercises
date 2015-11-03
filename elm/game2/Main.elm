import Graphics.Element exposing (container, middle, Element)
import Graphics.Collage exposing (rect, collage, filled)
import Color exposing (rgb)

-- MAIN
main = view modelInit

-- INIT

modelInit = Model (Position 20 20)

-- MODEL

type alias Position =
    { x : Float
    , y : Float
    }

type alias Model =
    {squarePosition : Position
    }

-- VIEW
view : Model -> Element
view model = container 500 500 middle <|
    collage 500 500
        [rect model.squarePosition.x model.squarePosition.y |> filled (rgb 100 100 100)]


