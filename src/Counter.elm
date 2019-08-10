module Counter exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ value (String.fromInt model) ] []
        , button [ onClick Increment ] [ text "count" ]
        ]
