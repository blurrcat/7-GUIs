module CircleDrawer exposing (Model, Msg, init, subscriptions, update, view)

import CircleDrawer.Ports exposing (Point, mouseDownOn)
import Debug
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (Svg, circle, rect, svg)
import Svg.Attributes as SA



-- MODEL


type alias Model =
    { clicked : Maybe Point
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { clicked = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedOnCanvas (Maybe Point)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedOnCanvas maybePoint ->
            ( { model | clicked = Debug.log "clicked" maybePoint }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    mouseDownOn ClickedOnCanvas



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "width" "400px"
        , style "height" "400px"
        ]
        [ buttonsView
        , canvasView
        ]


buttonsView =
    div [ style "text-align" "center" ]
        [ button [] [ text "Undo" ]
        , button [] [ text "Redo" ]
        ]


canvasView =
    svg
        [ SA.viewBox "0 0 100 100"
        , SA.width "100%"
        , SA.height "100%"
        , style "border" "solid 1px #333333"
        ]
        []
