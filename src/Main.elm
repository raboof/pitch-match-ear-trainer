port module Main exposing (..)

import Browser
import Browser.Events

import Json.Decode as Decode exposing (field, int, map)
import Json.Encode as Encode

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type Msg = Start | MouseMoved Int

main =
  Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

port setSounds : Encode.Value -> Cmd msg
port startAudio : () -> Cmd msg

type MouseState = Up | Down Int
type Model = Init | Finding Int MouseState

setY : Model -> Int -> Model
setY model y =
  case model of
    Init -> Init
    Finding t _ -> Finding t (Down y)

init () = (Init, Cmd.none)

encodeSounds tGain tFreq pGain pFreq =
  Encode.object
    [ ("target", Encode.object
        [ ("gain", Encode.float tGain)
        , ("frequency", Encode.int tFreq)
        ]
      )
    , ("pointed", Encode.object
        [ ("gain", Encode.float pGain)
        , ("frequency", Encode.int pFreq)
        ]
      )
    ]

sounds model =
  case model of
    Init -> encodeSounds 0 0 0 0
    Finding targetFreq Up -> encodeSounds 0.4 targetFreq 0 0
    Finding targetFreq (Down pointedFreq) -> encodeSounds 0.4 targetFreq 0.4 pointedFreq

updateAndSetSounds : Model -> (Model, Cmd msg)
updateAndSetSounds model = (model, setSounds (sounds model))

update msg model =
  case msg of
    Start -> (Finding 800 Up, startAudio())
    MouseMoved y -> updateAndSetSounds ( setY model y )

view model =
    case model of
      Init -> button [ onClick Start ] [ text "Start!" ]
      Finding target Up -> div [] [ text (String.fromInt target) ]
      Finding target (Down y) -> div [] [ text (String.fromInt target), text (String.fromInt y) ]

subscriptions model =
  Browser.Events.onMouseMove (map MouseMoved (field "pageY" int))
