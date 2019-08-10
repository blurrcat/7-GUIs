module Crud exposing (main)

import Browser
import Crud.Contact as Contact exposing (Contact, Name, Surname)
import Crud.Db as Db exposing (DB)
import Crud.Style as Style
import Debug
import Html exposing (Html, button, div, input, label, option, select, text)
import Html.Attributes exposing (disabled, multiple, style, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Filter a =
    Maybe a


apply : (a -> Bool) -> Filter a -> Maybe Bool
apply match filter_ =
    Maybe.map match filter_


matchBySurname : Filter Surname -> Contact -> Maybe Bool
matchBySurname filter_ contact_ =
    let
        match surname =
            String.startsWith
                (Contact.surnameToString surname)
                (Contact.getSurname contact_)
    in
    apply match filter_


matchById : Filter Contact.Id -> Contact -> Maybe Bool
matchById filter_ contact_ =
    let
        match id =
            Contact.getId contact_ == Contact.idToString id
    in
    apply match filter_


type alias ContactsDB =
    DB Contact


type alias Model =
    { name : Name
    , surname : Surname
    , filter : Filter Surname
    , contactsDB : ContactsDB
    , selectedId : Filter Contact.Id
    }


init : Model
init =
    { name = Contact.name ""
    , surname = Contact.surname ""
    , contactsDB = Db.db
    , filter = Nothing
    , selectedId = Nothing
    }



-- UPDATE


type Msg
    = Noop
    | ChangedName Name
    | ChangedSurname Surname
    | ChangedFilter (Maybe Surname)
    | ClickedCreate
    | ClickedDelete
    | SelectedContact (Maybe Contact.Id)


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

        ClickedDelete ->
            onClickedDelete model

        ChangedFilter filter ->
            { model | filter = filter }

        SelectedContact id ->
            { model | selectedId = id }


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


onClickedDelete : Model -> Model
onClickedDelete model =
    { model
        | contactsDB =
            Db.delete
                (matchById model.selectedId >> Maybe.withDefault False)
                model.contactsDB
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


contactsView : Filter Surname -> ContactsDB -> Html Msg
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


contactsListView : ContactsDB -> Filter Surname -> Html Msg
contactsListView contactsDB filter =
    let
        contactView contact =
            option [ value (Contact.getId contact) ]
                [ text (fullName contact)
                ]

        fullName contact =
            Contact.getSurname contact
                ++ ", "
                ++ Contact.getName contact

        options =
            contactsDB
                |> Db.filter (matchBySurname filter >> Maybe.withDefault True)
                |> Db.map contactView
                |> Db.rows

        msg text =
            if text == "" then
                SelectedContact Nothing

            else
                SelectedContact (Just (Contact.id text))
    in
    select
        [ multiple True
        , Style.width "100%"
        , onInput msg
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

        canDelete =
            model.selectedId /= Nothing
    in
    div []
        [ btn ClickedCreate canCreate "Create"
        , btn Noop True "Update"
        , btn ClickedDelete canDelete "Delete"
        ]



-- HELPERS
