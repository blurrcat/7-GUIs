module FlightBooker exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, input, option, select, text)
import Html.Attributes exposing (disabled, selected, style, type_, value)
import Html.Events exposing (onInput)
import Iso8601
import Time



-- MODEL


type FlightType
    = OneWay
    | TwoWay


oneWay : String
oneWay =
    "OneWay"


twoWay : String
twoWay =
    "TwoWay"


flightType : String -> FlightType
flightType text =
    if text == oneWay then
        OneWay

    else
        TwoWay


type alias Flight =
    { type_ : FlightType
    , startDate : String
    , returnDate : String
    }


isValid : Flight -> Bool
isValid { type_, startDate, returnDate } =
    -- we assume the browser is doing proper validation of the date input
    -- as that's not the point of this test
    case type_ of
        OneWay ->
            startDate /= ""

        TwoWay ->
            -- check if return date is later than start date
            case ( Iso8601.toTime startDate, Iso8601.toTime returnDate ) of
                ( Ok startDatePosix, Ok endDatePosix ) ->
                    Time.posixToMillis endDatePosix > Time.posixToMillis startDatePosix

                _ ->
                    False


type alias Model =
    Flight


init : Model
init =
    { type_ = OneWay
    , startDate = ""
    , returnDate = ""
    }



-- UPDATE


type Msg
    = SelectedFlightType FlightType
    | ChangedStartDate String
    | ChangedReturnDate String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectedFlightType newType ->
            { model | type_ = newType }

        ChangedStartDate startDate ->
            { model | startDate = startDate }

        ChangedReturnDate returnDate ->
            { model | returnDate = returnDate }



-- VIEW


view : Model -> Html Msg
view model =
    let
        isReturnDateDisabled =
            model.type_ == OneWay
    in
    div []
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            ]
            [ flightTypeView model.type_
            , dateInputView False ChangedStartDate model.startDate
            , dateInputView isReturnDateDisabled ChangedReturnDate model.returnDate
            , buttonView model
            ]
        ]


flightTypeView : FlightType -> Html Msg
flightTypeView type_ =
    div []
        [ select [ onInput (SelectedFlightType << flightType) ]
            [ option [ value oneWay, selected (type_ == OneWay) ] [ text "one-way flight" ]
            , option [ value twoWay, selected (type_ == TwoWay) ] [ text "two-way flight" ]
            ]
        ]


dateInputView : Bool -> (String -> Msg) -> String -> Html Msg
dateInputView isDisabled msg date =
    div []
        [ input
            [ value date
            , type_ "date"
            , disabled isDisabled
            , onInput msg
            ]
            []
        ]


buttonView : Model -> Html Msg
buttonView model =
    let
        isBookButtonDisabled =
            not (isValid model)
    in
    div []
        [ button [ disabled isBookButtonDisabled ] [ text "Book" ] ]
