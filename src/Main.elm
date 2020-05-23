module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type Msg = Start | MouseMoved

main =
  Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

init _ = (0, Cmd.none)

update msg model =
  case msg of
    Start -> model + 1

view model =
  div []
    [ button [ onClick Start ] [ text "Start!" ]
    , div [] [ text (String.fromInt model) ]
    ]

decodeMouseMove _ = MouseMoved

subscriptions model =
  Browser.Events.onMouseMove decodeMouseMove
