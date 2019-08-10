module Timer exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Html, button, div, input, label, progress, text)
import Html.Attributes as HA exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Task
import Time exposing (Posix)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Timer =
    { durationMs : Int
    , elapsedMs : Int
    , lastTickAt : Maybe Posix
    }


type alias Model =
    Timer


init : () -> ( Model, Cmd Msg )
init _ =
    let
        default =
            { durationMs = 0
            , elapsedMs = 0
            , lastTickAt = Nothing
            }
    in
    ( default
    , initLastTick default
    )


initLastTick : Timer -> Cmd Msg
initLastTick timer =
    if shouldRun timer && timer.lastTickAt == Nothing then
        Task.perform GotFirstTick Time.now

    else
        Cmd.none


shouldRun : Timer -> Bool
shouldRun timer =
    timer.durationMs > 0 && timer.durationMs > timer.elapsedMs



-- UPDATE


type Msg
    = ChangedDuration Int
    | Ticked Posix
    | GotFirstTick Posix
    | Reset
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedDuration durationMs ->
            ( { model | durationMs = durationMs }, initLastTick model )

        GotFirstTick now ->
            ( { model | lastTickAt = Just now }, Cmd.none )

        Ticked now ->
            updateTick now model

        Reset ->
            ( { model | elapsedMs = 0 }, Cmd.none )

        Noop ->
            ( model, Cmd.none )


updateTick : Posix -> Model -> ( Model, Cmd Msg )
updateTick now model =
    let
        elapsedMs =
            model.lastTickAt
                |> Maybe.map (diffPosix now)
                |> Maybe.withDefault 0
                |> (+) model.elapsedMs

        lastTickAt =
            if shouldRun model then
                Just now

            else
                Nothing
    in
    ( { model
        | elapsedMs = elapsedMs
        , lastTickAt = lastTickAt
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions timer =
    if shouldRun timer && timer.lastTickAt /= Nothing then
        onAnimationFrame Ticked

    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ elapsedTimeView model.elapsedMs model.durationMs
        , durationSliderView model.durationMs
        , resetButtonView
        ]


elapsedTimeView : Int -> Int -> Html msg
elapsedTimeView elapsedMs durationMs =
    let
        elapsedSec =
            msToSec elapsedMs

        durationSec =
            msToSec durationMs

        percentage =
            if durationMs == 0 then
                ""

            else
                String.fromFloat (elapsedSec / durationSec)

        elapsed =
            String.fromFloat elapsedSec ++ "s"
    in
    div []
        [ div []
            [ label labelStyles [ text "Elapsed Time:" ]
            , progress [ value percentage ] []
            ]
        , div []
            [ label labelStyles [ text elapsed ]
            ]
        ]


durationSliderView : Int -> Html Msg
durationSliderView durationMs =
    let
        msg text =
            case String.toInt text of
                Just ms ->
                    ChangedDuration ms

                Nothing ->
                    Noop

        duration =
            String.fromFloat (msToSec durationMs) ++ "s"
    in
    div []
        [ div []
            [ label labelStyles [ text "Duration" ]
            , input
                [ value (String.fromInt durationMs)
                , type_ "range"
                , HA.min "0"
                , HA.max (String.fromInt maxDurationInMs)
                , HA.step "1"
                , onInput msg
                ]
                []
            ]
        , div []
            [ label labelStyles [ text duration ]
            ]
        ]


resetButtonView : Html Msg
resetButtonView =
    div []
        [ button [ onClick Reset ] [ text "Reset" ]
        ]


labelStyles : List (Html.Attribute msg)
labelStyles =
    [ style "display" "inline-block"
    , style "width" "120px"
    ]



--- HELPERS


diffPosix : Posix -> Posix -> Int
diffPosix p1 p2 =
    Time.posixToMillis p1 - Time.posixToMillis p2


maxDurationInMs : Int
maxDurationInMs =
    60 * 1000


msToSec : Int -> Float
msToSec ms =
    toFloat ms / 1000
