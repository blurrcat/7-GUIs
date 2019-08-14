module Main exposing (main)

import Browser
import Counter
import Crud
import FlightBooker
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import TemperatureConverter
import Timer


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { counter : Counter.Model
    , temperatureConverter : TemperatureConverter.Model
    , timer : Timer.Model
    , flightBooker : FlightBooker.Model
    , crud : Crud.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( timer, cmd ) =
            Timer.init ()
    in
    ( { counter = Counter.init
      , temperatureConverter = TemperatureConverter.init
      , timer = timer
      , flightBooker = FlightBooker.init
      , crud = Crud.init
      }
    , Cmd.map GotTimerMsg cmd
    )



-- UPDATE


type Msg
    = Noop
    | GotCounterMsg Counter.Msg
    | GotTemperatureConverterMsg TemperatureConverter.Msg
    | GotTimerMsg Timer.Msg
    | GotCrudMsg Crud.Msg
    | GotFlightBookerMsg FlightBooker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        GotCounterMsg msg_ ->
            ( { model | counter = Counter.update msg_ model.counter }, Cmd.none )

        GotTemperatureConverterMsg msg_ ->
            ( { model | temperatureConverter = TemperatureConverter.update msg_ model.temperatureConverter }, Cmd.none )

        GotCrudMsg msg_ ->
            ( { model | crud = Crud.update msg_ model.crud }, Cmd.none )

        GotFlightBookerMsg msg_ ->
            ( { model | flightBooker = FlightBooker.update msg_ model.flightBooker }, Cmd.none )

        GotTimerMsg msg_ ->
            let
                ( timer, cmd ) =
                    Timer.update msg_ model.timer
            in
            ( { model | timer = timer }, Cmd.map GotTimerMsg cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map GotTimerMsg (Timer.subscriptions model.timer)



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "margin" "0 auto"
        , style "min-width" "600px"
        ]
        [ sectionView GotCounterMsg (Counter.view model.counter)
        , sectionView GotTemperatureConverterMsg (TemperatureConverter.view model.temperatureConverter)
        , sectionView GotFlightBookerMsg (FlightBooker.view model.flightBooker)
        , sectionView GotTimerMsg (Timer.view model.timer)
        , sectionView GotCrudMsg (Crud.view model.crud)
        ]


sectionView : (a -> Msg) -> Html a -> Html Msg
sectionView subMsg html =
    div [ style "margin" "2em" ] [ Html.map subMsg html ]
