port module Main exposing (..)

import List exposing (head)
import Maybe exposing (withDefault)

import Browser
import Browser.Events

import Json.Decode as Decode exposing (field, int, string, map, list)
import Json.Encode as Encode

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, on)

type alias Frequency = Int

type PointState = Up | Down Frequency

type Model = Init | Finding Int PointState
type Msg = Start | MouseMoved Int | MouseUp

main =
  Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

port setSounds : Encode.Value -> Cmd msg
port startAudio : () -> Cmd msg
port onMouseEnter : ({ pageY : Int } -> msg) -> Sub msg
port onMouseOut : ({} -> msg) -> Sub msg
port onTouchStart : (Decode.Value -> msg) -> Sub msg
port onTouchMove : (Decode.Value -> msg) -> Sub msg
port onTouchCancel : ({} -> msg) -> Sub msg
port onTouchEnd : ({} -> msg) -> Sub msg

setY : Model -> Int -> Model
setY model y =
  case model of
    Init -> Init
    Finding t _ -> Finding t (Down y)

up model =
  case model of
    Init -> Init
    Finding t _ -> Finding t Up

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
    MouseMoved y -> updateAndSetSounds (setY model y)
    MouseUp -> updateAndSetSounds (up model)

view model =
    case model of
      Init -> button [ onClick Start ] [ text "Start!" ]
      Finding target Up -> div [] [ text (String.fromInt target) ]
      Finding target (Down y) -> div [] [ text (String.fromInt target), text (String.fromInt y) ]

getTouchY obj =
  case (Decode.decodeValue (field "touches" (field "0" (field "pageY" int))) obj) of
    Ok y -> MouseMoved y
    Err error -> MouseMoved 0

subscriptions _ = Sub.batch
  [ Browser.Events.onMouseMove (map MouseMoved (field "pageY" int))
  , onMouseEnter (\o -> (MouseMoved o.pageY))
  , onTouchStart getTouchY
  , onTouchMove getTouchY
  , onTouchEnd (\_ -> MouseUp)
  , onMouseOut (\_ -> MouseUp)
  , onTouchCancel (\_ -> MouseUp)
  ]
