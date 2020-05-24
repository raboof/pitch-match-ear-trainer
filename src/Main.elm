port module Main exposing (..)

import List exposing (head)
import Maybe exposing (withDefault)

import Browser
import Browser.Events

import Json.Decode as Decode exposing (..)
import Json.Encode as Encode

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (attribute)

type alias Frequency = Int

type PointState = Up | Down Frequency

type Page = 
  Init |
  Calibrating |
  Trying PointState |
  Finding Int PointState |
  ErrorPage String

type alias Model = 
  { windowHeight : Int
  , currentPage : Page
  }
    
type Msg =
  Calibrate |
  Try |
  Start |
  MouseMoved Int |
  MouseUp |
  Error String

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

minPitch = 300.0
maxPitch = 2000.0

yToFrequency : Int -> Int -> Frequency
yToFrequency windowHeight y = round (maxPitch - ((maxPitch - minPitch) * (toFloat y) / (toFloat windowHeight)))

setY : Model -> Int -> Page
setY model y =
  case model.currentPage of
    Init -> Init
    Calibrating -> Calibrating
    Trying _ -> Trying (Down (yToFrequency model.windowHeight y))
    Finding t _ -> Finding t (Down (yToFrequency model.windowHeight y))
    ErrorPage e -> ErrorPage e

up model =
  case model of
    Init -> Init
    Calibrating -> Calibrating
    Trying _ -> Trying Up
    Finding t _ -> Finding t Up
    ErrorPage e -> ErrorPage e

init windowHeight = ({ windowHeight = windowHeight, currentPage = Init }, Cmd.none)

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

silent = encodeSounds 0 0 0 0

sounds model =
  case model of
    Init -> silent
    Calibrating -> encodeSounds 0.4 800 0 0
    Trying (Down pointedFreq) -> encodeSounds 0 0 0.4 pointedFreq
    Trying Up -> silent
    Finding targetFreq Up -> encodeSounds 0.4 targetFreq 0 0
    Finding targetFreq (Down pointedFreq) -> encodeSounds 0.4 targetFreq 0.4 pointedFreq
    ErrorPage e -> silent

updateAndSetSounds : Model -> (Model, Cmd msg)
updateAndSetSounds model = (model, setSounds (sounds model.currentPage))

updateModel model msg =
  -- Later split out per current page type
  case msg of
    Calibrate -> { model | currentPage = Calibrating }
    Try -> { model | currentPage = Trying Up }
    Start -> { model | currentPage = Finding 800 Up }
    MouseMoved y -> { model | currentPage = setY model y }
    MouseUp -> { model | currentPage = up model.currentPage }
    Error e -> { model | currentPage = ErrorPage e }

update : Msg -> Model -> (Model, Cmd msg)
update msg model = updateAndSetSounds (updateModel model msg)

view model =
  case model.currentPage of
    Init -> div []
      [ div [ attribute "class" "text" ] [ text "This game helps you train 'pitch matching' by ear. It is best enjoyed alone or with headphones." ] 
      , div [ onClick Calibrate, attribute "class" "button" ] [ text "Calibrate volume" ]
      ]
    Calibrating -> div []
      [ div [ attribute "class" "text" ] [ text "You should now hear a 'target pitch'. Later you will be challenged to match this pitch. Take a minute to adjust your volume so it is comfortable" ] 
      , div [ onClick Try, attribute "class" "button" ] [ text "I'm happy" ]
      ]
    Trying _ -> div []
      [ div [ attribute "class" "text" ] [ text "You can 'play' by touching the screen. Try it!" ] 
      , div [ onClick Start, attribute "class" "button" ] [ text "Got it!" ]
      ]
    Finding target Up -> div [] [ text (String.fromInt target) ]
    Finding target (Down pointed) ->
      div
        []
        [ text "target "
        , text (String.fromInt target)
        , text "pointed "
        , text (String.fromInt pointed)
        , text "diff "
        , text (String.fromInt (abs (target - pointed)))
        , div [ attribute "class" "text" ] [ text "Try to match the 2 pitches" ]
        , if abs (target - pointed) < 7
          then div [] [ text "Match!" ] 
          else div [] [ text "No match yet..." ] 
        ]
    ErrorPage e -> div [] 
      [ div [ attribute "class" "text" ] [ text e ] ]

getTouchY obj =
  case (Decode.decodeValue (field "touches" (field "0" (field "pageY" float))) obj) of
    Ok y -> MouseMoved (round y)
    Err error -> Error (errorToString error)

subscriptions _ = Sub.batch
  [ Browser.Events.onMouseMove (map MouseMoved (field "pageY" int))
  , onMouseEnter (\o -> (MouseMoved o.pageY))
  , onTouchStart getTouchY
  , onTouchMove getTouchY
  , onTouchEnd (\_ -> MouseUp)
  , onMouseOut (\_ -> MouseUp)
  , onTouchCancel (\_ -> MouseUp)
  ]
