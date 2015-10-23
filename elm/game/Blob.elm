module Blob where

import Effects exposing (Effects, Never)
import Html exposing (..)



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
  div []
    [ text "Hellooboo" ]

-- EFFECTS
-- none :)

