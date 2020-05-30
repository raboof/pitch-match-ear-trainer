port module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
import List exposing (head)
import Maybe exposing (withDefault)
import Random
import Time


type alias Frequency =
    Int


type alias GoingFor =
    Int


type alias OkFor =
    Int


type PointState
    = Up
    | Down Frequency


type Page
    = Init
    | Calibrating
    | Trying GoingFor PointState
    | Finding Frequency OkFor PointState
    | Found
    | ErrorPage String


type alias Model =
    { windowHeight : Int
    , muted : Bool
    , page : Page
    }


type Msg
    = Calibrate
    | Try
    | Start
    | NewChallenge Frequency
    | MouseMoved Int
    | MouseUp
    | Error String
    | Tick
    | ToggleMute


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port setSounds : Encode.Value -> Cmd msg


port success : () -> Cmd msg


port onMouseEnter : ({ pageY : Int } -> msg) -> Sub msg


port onMouseOut : ({} -> msg) -> Sub msg


port onTouchStart : (Decode.Value -> msg) -> Sub msg


port onTouchMove : (Decode.Value -> msg) -> Sub msg


port onTouchCancel : ({} -> msg) -> Sub msg


port onTouchEnd : ({} -> msg) -> Sub msg


minPitch =
    300


maxPitch =
    1000


yToFrequency : Int -> Int -> Frequency
yToFrequency windowHeight y =
    round (maxPitch - ((maxPitch - minPitch) * toFloat y / toFloat windowHeight))


matches : Frequency -> Frequency -> Bool
matches target current =
    abs (target - current) < 7


setY : Model -> Int -> Page
setY model y =
    case model.page of
        Init ->
            Init

        Calibrating ->
            Calibrating

        Trying goingFor _ ->
            Trying goingFor (Down (yToFrequency model.windowHeight y))

        Finding target okFor _ ->
            let
                current =
                    yToFrequency model.windowHeight y
            in
            Finding target okFor (Down current)

        Found ->
            Found

        ErrorPage e ->
            ErrorPage e


up model =
    case model of
        Init ->
            Init

        Calibrating ->
            Calibrating

        Trying goingFor _ ->
            Trying goingFor Up

        Finding t _ _ ->
            Finding t 0 Up

        Found ->
            Found

        ErrorPage e ->
            ErrorPage e


init windowHeight =
    ( { windowHeight = windowHeight
      , muted = False
      , page = Init
      }
    , Cmd.none
    )


encodeSounds tGain tFreq pGain pFreq =
    Encode.object
        [ ( "target"
          , Encode.object
                [ ( "gain", Encode.float tGain )
                , ( "frequency", Encode.int tFreq )
                ]
          )
        , ( "pointed"
          , Encode.object
                [ ( "gain", Encode.float pGain )
                , ( "frequency", Encode.int pFreq )
                ]
          )
        ]


silent =
    encodeSounds 0 0 0 0


sounds model =
    if model.muted then
        silent

    else
        case model.page of
            Init ->
                silent

            Calibrating ->
                encodeSounds 0.4 800 0 0

            Trying _ (Down pointedFreq) ->
                encodeSounds 0 0 0.4 pointedFreq

            Trying _ Up ->
                silent

            Finding targetFreq _ Up ->
                encodeSounds 0.4 targetFreq 0 0

            Finding targetFreq _ (Down pointedFreq) ->
                encodeSounds 0.4 targetFreq 0.4 pointedFreq

            Found ->
                silent

            ErrorPage e ->
                silent


tick page =
    case page of
        Trying goingFor (Down freq) ->
            Trying (goingFor + 1) (Down freq)

        Finding target okFor (Down current) ->
            if okFor > 5 then
                Found

            else if matches target current then
                Finding target (okFor + 1) (Down current)

            else
                Finding target 0 (Down current)

        _ ->
            page


updateModel model msg =
    case msg of
        Calibrate ->
            { model | page = Calibrating }

        Try ->
            { model | page = Trying 0 Up }

        Start ->
            model

        NewChallenge target ->
            { model | page = Finding target 0 Up }

        MouseMoved y ->
            { model | page = setY model y }

        MouseUp ->
            { model | page = up model.page }

        Error e ->
            { model | page = ErrorPage e }

        Tick ->
            { model | page = tick model.page }

        ToggleMute ->
            { model | muted = not model.muted }


effect msg =
    case msg of
        Start ->
            Random.generate NewChallenge (Random.int (minPitch + 10) (maxPitch - 10))

        _ ->
            Cmd.none


transition from to =
    case ( from, to ) of
        ( _, Found ) ->
            if from /= Found then
                success ()

            else
                Cmd.none

        _ ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updated =
            updateModel model msg
    in
    ( updated, Cmd.batch [ effect msg, transition model.page updated.page, setSounds (sounds updated) ] )


view model =
    case model.page of
        Init ->
            div []
                [ div [ attribute "class" "text" ] [ text "This game helps you train 'pitch matching' by ear. It is best enjoyed alone or with headphones." ]
                , div [ onClick Calibrate, attribute "class" "button" ] [ text "Calibrate volume" ]
                ]

        Calibrating ->
            div []
                [ div [ attribute "class" "text" ] [ text "You should now hear a 'target pitch'. Later you will be challenged to match this pitch. Take a minute to adjust your volume so it is comfortable" ]
                , div [ onClick Try, attribute "class" "button" ] [ text "I'm happy" ]
                ]

        Trying goingFor _ ->
            div []
                [ div [ attribute "class" "text" ] [ text "You can 'play' by touching the screen. Try it!" ]
                , div
                    [ if goingFor > 3 then
                        onClick Start

                      else
                        attribute "class" "disabled"
                    , attribute "class" "button"
                    ]
                    [ text "Got it!" ]
                , div [ attribute "class" "huge" ]
                    [ text
                        (if goingFor < 2 then
                            "👆"

                         else
                            ""
                        )
                    ]
                ]

        --Finding target _ Up -> div [] [ text (String.fromInt target) ]
        --Finding target okFor (Down pointed) ->
        Finding target _ _ ->
            div
                []
                [ text "target "
                , text (String.fromInt target)

                --, text "pointed "
                --, text (String.fromInt pointed)
                --, text "diff "
                --, text (String.fromInt (abs (target - pointed)))
                --, text " ok for: "
                --, text (String.fromInt okFor)
                , div [ attribute "class" "text" ] [ text "Try to match the 2 pitches" ]

                --, if matches target pointed
                --  then div [] [ text "Match!" ]
                --  else div [] [ text "No match yet..." ]
                , div [ onClick ToggleMute, attribute "class" "mute" ]
                    [ text
                        (if model.muted then
                            "🔇"

                         else
                            "🔉"
                        )
                    ]
                ]

        Found ->
            div []
                [ div [ attribute "class" "text" ] [ text "Nice! You got it!" ]
                , div [ onClick Start, attribute "class" "button" ] [ text "Play another" ]
                ]

        ErrorPage e ->
            div []
                [ div [ attribute "class" "text" ] [ text e ] ]


getTouchY obj =
    case Decode.decodeValue (field "touches" (field "0" (field "pageY" float))) obj of
        Ok y ->
            MouseMoved (round y)

        Err error ->
            Error (errorToString error)


subscriptions _ =
    Sub.batch
        [ Browser.Events.onMouseMove (map MouseMoved (field "pageY" int))
        , onMouseEnter (\o -> MouseMoved o.pageY)
        , onTouchStart getTouchY
        , onTouchMove getTouchY
        , onTouchEnd (\_ -> MouseUp)
        , onMouseOut (\_ -> MouseUp)
        , onTouchCancel (\_ -> MouseUp)
        , Time.every 200 (\_ -> Tick)
        ]
