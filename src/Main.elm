module Main exposing (..)

import Browser
import Browser.Events

import Json.Decode as D

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

init () = (0, Cmd.none)

update msg model =
  case msg of
    Start -> (model + 1, Cmd.none)
    MouseMoved -> (model - 1, Cmd.none)

view model =
  div []
    [ button [ onClick Start ] [ text "Start!" ]
    , div [] [ text (String.fromInt model) ]
    ]

subscriptions model =
  Browser.Events.onMouseMove (D.succeed MouseMoved)
