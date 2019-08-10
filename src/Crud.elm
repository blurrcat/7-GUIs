module Crud exposing (main)

-- import Debug

import Browser
import Crud.Contact as Contact exposing (Contact, Name, Surname)
import Crud.Db as Db exposing (DB)
import Crud.Style as Style
import Html exposing (Html, button, div, input, label, option, select, text)
import Html.Attributes exposing (disabled, multiple, style, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Filter =
    Maybe Surname


matchContact : Filter -> Contact -> Bool
matchContact filter_ contact_ =
    let
        match surname =
            String.startsWith (Contact.surnameToString surname) (Contact.getSurname contact_)
    in
    filter_
        |> Maybe.map match
        |> Maybe.withDefault True


type alias ContactsDB =
    DB Contact


type alias Model =
    { name : Name
    , surname : Surname
    , filter : Filter
    , contactsDB : ContactsDB
    }


init : Model
init =
    { name = Contact.name ""
    , surname = Contact.surname ""
    , contactsDB = Db.db
    , filter = Nothing
    }



-- UPDATE


type Msg
    = Noop
    | ChangedName Name
    | ChangedSurname Surname
    | ChangedFilter (Maybe Surname)
    | ClickedCreate


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model

        ChangedName name ->
            { model | name = name }

        ChangedSurname surname ->
            { model | surname = surname }

        ClickedCreate ->
            onClickedCreate model

        ChangedFilter filter ->
            { model | filter = filter }


onClickedCreate : Model -> Model
onClickedCreate model =
    let
        newContact =
            Contact.id >> Contact.contact model.name model.surname
    in
    { model
        | name = Contact.name ""
        , surname = Contact.surname ""
        , contactsDB = Db.insert newContact model.contactsDB
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div
            [ Style.display "flex"
            , style "flex-direction" "row"
            ]
            [ contactsView model.filter model.contactsDB
            , newContactView model.name model.surname
            ]
        , controlsView model
        ]


contactsView : Filter -> ContactsDB -> Html Msg
contactsView filter contactsDB =
    div
        [ style "margin" "0 1em 1em 0"
        ]
        [ filterView
        , contactsListView contactsDB filter
        ]


filterView : Html Msg
filterView =
    let
        msg text =
            if text == "" then
                ChangedFilter Nothing

            else
                ChangedFilter (Just (Contact.surname text))
    in
    div []
        [ label [] [ text "Filter prefix:" ]
        , input [ onInput msg ] []
        ]


contactsListView : ContactsDB -> Filter -> Html msg
contactsListView contactsDB filter =
    let
        fullName contact =
            Contact.getSurname contact
                ++ ", "
                ++ Contact.getName contact

        contactView contact =
            option [ value (Contact.getId contact) ]
                [ text (fullName contact)
                ]

        options =
            contactsDB
                |> Db.filter (matchContact filter)
                |> Db.map contactView
                |> Db.rows
    in
    select
        [ multiple True
        , Style.width "100%"
        ]
        options


newContactView : Name -> Surname -> Html Msg
newContactView name surname =
    div []
        [ inputView
            (Contact.name >> ChangedName)
            "Name"
            (Contact.nameToString name)
        , inputView
            (Contact.surname >> ChangedSurname)
            "Surname"
            (Contact.surnameToString surname)
        ]


inputView : (String -> Msg) -> String -> String -> Html Msg
inputView msg name value_ =
    div []
        [ label
            [ Style.display "inline-block"
            , Style.width "4em"
            ]
            [ text (name ++ ":") ]
        , input
            [ value value_
            , onInput msg
            ]
            []
        ]


controlsView : Model -> Html Msg
controlsView model =
    let
        btn msg enabled text_ =
            button
                [ style "padding-right" "1em"
                , disabled (not enabled)
                , onClick msg
                ]
                [ text text_ ]

        canCreate =
            (Contact.nameToString model.name /= "")
                && (Contact.surnameToString model.surname /= "")
    in
    div []
        [ btn ClickedCreate canCreate "Create"
        , btn Noop True "Update"
        , btn Noop True "Delete"
        ]



-- HELPERS
