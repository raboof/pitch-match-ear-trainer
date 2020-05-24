port module Main exposing (..)

import Browser
import Browser.Events

import Json.Decode as Decode
import Json.Encode as Encode

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

port setSounds : Encode.Value -> Cmd msg
port startAudio : () -> Cmd msg

init () = (0, Cmd.none)

sounds model =
  Encode.object
    [ ("target", Encode.object
        [ ("gain", Encode.float 0.4)
        , ("frequency", Encode.int 800)
        ]
      )
    , ("pointed", Encode.object
        [ ("gain", Encode.float 0.4)
        , ("frequency", Encode.int 900)
        ]
      )
    ]

update msg model =
  case msg of
    Start -> (model + 1, startAudio())
    MouseMoved -> (model - 1, setSounds (sounds model))

view model =
  div []
    [ button [ onClick Start ] [ text "Start!" ]
    , div [] [ text (String.fromInt model) ]
    ]

subscriptions model =
  Browser.Events.onMouseMove (Decode.succeed MouseMoved)
