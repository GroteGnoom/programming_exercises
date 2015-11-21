import Graphics.Element exposing (container, bottomLeft, Element)
import Graphics.Collage exposing (rect, collage, filled)
import Color exposing (rgb)

main : Element
main = view

-- MODEL

-- VIEW
view : Element
view = container 500 500 bottomLeft <|
    collage 500 500
        [(rect 20 20 |>
                filled (rgb 100 100 100)
        )]
        
