module TemperatureConverter exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (required, step, type_, value)
import Html.Events exposing (onInput)


type alias Model =
    { celsius : String
    , fahrenheit : String
    }


init : Model
init =
    { celsius = ""
    , fahrenheit = ""
    }



-- UPDATE


type Msg
    = ChangedCelsius String
    | ChangedFahrenheit String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangedCelsius text ->
            updateCelsius text

        ChangedFahrenheit text ->
            updateFahrenheit text


updateCelsius : String -> Model
updateCelsius text =
    case String.toFloat text of
        Just celsius ->
            { celsius = text
            , fahrenheit = String.fromFloat (toFahrenheit celsius)
            }

        Nothing ->
            { celsius = text
            , fahrenheit = ""
            }


updateFahrenheit : String -> Model
updateFahrenheit text =
    case String.toFloat text of
        Just fahrenheit ->
            { celsius = String.fromFloat (toCelsius fahrenheit)
            , fahrenheit = text
            }

        Nothing ->
            { celsius = ""
            , fahrenheit = text
            }


toFahrenheit : Float -> Float
toFahrenheit celsius =
    celsius * (9 / 5) + 32


toCelsius : Float -> Float
toCelsius fahrenheit =
    (fahrenheit - 32) * (5 / 9)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ temperatureView "Celsius" ChangedCelsius model.celsius
        , text " = "
        , temperatureView "Fahrenheit" ChangedFahrenheit model.fahrenheit
        ]


temperatureView : String -> (String -> Msg) -> String -> Html Msg
temperatureView name msg temperature =
    span []
        [ input
            [ value temperature
            , onInput msg
            , required True
            , type_ "number"
            , step "any"
            ]
            []
        , text (" " ++ name)
        ]
