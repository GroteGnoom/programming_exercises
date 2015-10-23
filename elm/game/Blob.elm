module Blob where

import Effects exposing (Effects, Never)
import Html exposing (Html)
import Svg exposing (svg, rect, g, text, polygon)
import Svg.Attributes exposing (..)


-- MODEL

type alias Model =
    (Int, Int)


init : (Model, Effects Action)
init =
  ( (100, 100)
  , Effects.none
  )


-- UPDATE

type Action
    = NoAction


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoAction ->
      (model, Effects.none)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
--  div []
--    [ text "Hellooboo" ]
  svg [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95" ]
    [ polygon [ fill "#F0AD00", points "161.649,152.782 231.514,82.916 91.783,82.916" ] []
    ]



-- EFFECTS
-- none :)

