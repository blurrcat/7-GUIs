module Crud exposing (main)

import Browser
import Crud.Contact as Contact exposing (Contact)
import Crud.Style as Style
import Debug
import Html exposing (Html, button, div, input, label, option, select, text)
import Html.Attributes exposing (multiple, style, value)
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
        [ div
            [ Style.display "flex"
            , style "flex-direction" "row"
            ]
            [ contactsView
            , newContactView
            ]
        , controlsView
        ]


contactsView =
    div
        [ style "margin" "0 1em 1em 0"
        ]
        [ filterView
        , contactsListView []
        ]


filterView =
    div []
        [ label [] [ text "Filter prefix:" ]
        , input [] []
        ]


contactsListView : List Contact -> Html msg
contactsListView contacts =
    let
        fullName contact =
            Contact.surname contact ++ ", " ++ Contact.name contact

        contactView contact =
            option [ value (Contact.id contact) ]
                [ text (fullName contact)
                ]

        options =
            contacts
                |> List.map contactView
    in
    select
        [ multiple True
        , Style.width "100%"
        ]
        options


newContactView =
    div []
        [ inputView "Name" "John"
        , inputView "Surname" "Romba"
        ]


inputView : String -> String -> Html msg
inputView name value_ =
    div []
        [ label
            [ Style.display "inline-block"
            , Style.width "4em"
            ]
            [ text (name ++ ":") ]
        , input [ value value_ ] []
        ]


controlsView =
    let
        btn text_ =
            button
                [ style "padding-right" "1em"
                ]
                [ text text_ ]
    in
    div []
        [ btn "Create"
        , btn "Update"
        , btn "Delete"
        ]



-- HELPERS
