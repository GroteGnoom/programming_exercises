import Graphics.Element exposing (container, middle, Element)
import Graphics.Collage exposing (rect, collage, filled)
import Color exposing (rgb)

main = view

view : Element
view = container 100 100 middle <|
    collage 100 100
        [rect 20 20 |> filled (rgb 100 100 100)]

