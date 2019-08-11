module Crud exposing (main)

import Browser
import Crud.Contact as Contact exposing (Contact, Name, Surname)
import Crud.Db as Db exposing (DB)
import Crud.Db.Row as Row exposing (Row)
import Crud.Style as Style
import Html exposing (Html, button, div, input, label, option, select, text)
import Html.Attributes exposing (disabled, multiple, style, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Filter a =
    Maybe a


type alias ContactsDB =
    DB Contact


type alias ContactRow =
    Row Contact


type alias Model =
    { name : Name
    , surname : Surname
    , filter : Filter Surname
    , contactsDB : ContactsDB
    , selectedId : Maybe Row.Id
    , selectedContact : Maybe ContactRow
    }


init : Model
init =
    { name = Contact.name ""
    , surname = Contact.surname ""
    , contactsDB = Db.db
    , filter = Nothing
    , selectedId = Nothing
    , selectedContact = Nothing
    }



-- UPDATE


type Msg
    = ChangedName Name
    | ChangedSurname Surname
    | ChangedFilter (Maybe Surname)
    | ClickedCreate
    | ClickedDelete
    | ClickedUpdate
    | SelectedContact (Maybe Row.Id)


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangedName name ->
            { model | name = name }

        ChangedSurname surname ->
            { model | surname = surname }

        ClickedCreate ->
            onClickedCreate model

        ClickedDelete ->
            onClickedDelete model

        ClickedUpdate ->
            onClickedUpdate model

        ChangedFilter filter ->
            { model | filter = filter }

        SelectedContact id ->
            onSelectedContact id model


onClickedCreate : Model -> Model
onClickedCreate model =
    let
        newContact =
            Contact.contact model.name model.surname
    in
    { model
        | name = Contact.name ""
        , surname = Contact.surname ""
        , contactsDB = Db.insert newContact model.contactsDB
        , selectedId = Nothing
    }


onClickedUpdate : Model -> Model
onClickedUpdate model =
    let
        newContact =
            Contact.contact model.name model.surname
    in
    { model
        | contactsDB =
            model.selectedId
                |> Maybe.map (\id -> Db.update id newContact model.contactsDB)
                |> Maybe.withDefault model.contactsDB
    }


onClickedDelete : Model -> Model
onClickedDelete model =
    { model
        | contactsDB =
            model.selectedId
                |> Maybe.map (\id -> Db.delete id model.contactsDB)
                |> Maybe.withDefault model.contactsDB
        , selectedId = Nothing
    }


onSelectedContact : Maybe Row.Id -> Model -> Model
onSelectedContact maybeId model =
    let
        maybeRow =
            maybeId
                |> Maybe.andThen (\id -> Db.get id model.contactsDB)
    in
    case maybeRow of
        Nothing ->
            { model
                | selectedId = maybeId
                , name = Contact.name ""
                , surname = Contact.surname ""
            }

        Just row ->
            { model
                | selectedId = maybeId
                , name = Contact.getName row.data
                , surname = Contact.getSurname row.data
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
contactsListView db filter =
    let
        contactView row =
            option [ value (getIdAsString row) ]
                [ text (fullName row.data)
                ]

        fullName contact =
            getSurnameAsString contact
                ++ ", "
                ++ getNameAsString contact

        db_ =
            case filter of
                Nothing ->
                    db

                Just surname ->
                    let
                        match contact =
                            String.startsWith
                                (Contact.surnameToString surname)
                                (getSurnameAsString contact)
                    in
                    Db.filter match db

        options =
            db_
                |> Db.rows
                |> List.map contactView

        msg text =
            if text == "" then
                SelectedContact Nothing

            else
                SelectedContact (Just (Row.id text))
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

        contactExists =
            Db.exists (Contact.contact model.name model.surname) model.contactsDB

        canCreate =
            (Contact.nameToString model.name /= "")
                && (Contact.surnameToString model.surname /= "")
                && not contactExists

        isSelected =
            model.selectedId /= Nothing

        canUpdate =
            canCreate && isSelected
    in
    div []
        [ btn ClickedCreate canCreate "Create"
        , btn ClickedUpdate canUpdate "Update"
        , btn ClickedDelete isSelected "Delete"
        ]



-- HELPERS


getSurnameAsString : Contact -> String
getSurnameAsString =
    Contact.getSurname >> Contact.surnameToString


getNameAsString : Contact -> String
getNameAsString =
    Contact.getName >> Contact.nameToString


getIdAsString : Row Contact -> String
getIdAsString =
    .id >> Row.idToString
