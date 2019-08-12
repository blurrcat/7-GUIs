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


type alias Model =
    { name : Name
    , surname : Surname
    , surnameFilter : SurnameFilter
    , contactDB : ContactDB
    , selectedId : Maybe Row.Id
    }


type alias ContactDB =
    DB Contact


type alias ContactRow =
    Row Contact


type alias SurnameFilter =
    Maybe Surname


init : Model
init =
    { name = ""
    , surname = ""
    , contactDB = Db.db
    , surnameFilter = Nothing
    , selectedId = Nothing
    }



-- UPDATE


type Msg
    = ChangedName Name
    | ChangedSurname Surname
    | ChangedSurnameFilter SurnameFilter
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

        ChangedSurnameFilter surnameFilter ->
            { model | surnameFilter = surnameFilter }

        SelectedContact id ->
            onSelectedContact id model


onClickedCreate : Model -> Model
onClickedCreate model =
    let
        newContact =
            Contact.contact model.name model.surname
    in
    { model
        | name = ""
        , surname = ""
        , contactDB = Db.insert newContact model.contactDB
        , selectedId = Nothing
    }


onClickedUpdate : Model -> Model
onClickedUpdate model =
    let
        newContact =
            Contact.contact model.name model.surname
    in
    { model
        | contactDB =
            model.selectedId
                |> Maybe.map (\id -> Db.update id newContact model.contactDB)
                |> Maybe.withDefault model.contactDB
    }


onClickedDelete : Model -> Model
onClickedDelete model =
    { model
        | contactDB =
            model.selectedId
                |> Maybe.map (\id -> Db.delete id model.contactDB)
                |> Maybe.withDefault model.contactDB
        , selectedId = Nothing
    }


onSelectedContact : Maybe Row.Id -> Model -> Model
onSelectedContact maybeId model =
    let
        maybeRow =
            maybeId
                |> Maybe.andThen (\id -> Db.get id model.contactDB)
    in
    case maybeRow of
        Nothing ->
            { model
                | selectedId = maybeId
                , name = ""
                , surname = ""
            }

        Just row ->
            { model
                | selectedId = maybeId
                , name = Contact.name row.data
                , surname = Contact.surname row.data
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div
            [ Style.display "flex"
            , style "flex-direction" "row"
            ]
            [ contactsView model.surnameFilter model.contactDB
            , newContactView model.name model.surname
            ]
        , controlsView model
        ]


contactsView : SurnameFilter -> ContactDB -> Html Msg
contactsView filter db =
    div
        [ style "margin" "0 1em 1em 0"
        ]
        [ filterView
        , contactsListView db filter
        ]


filterView : Html Msg
filterView =
    let
        msg text =
            if text == "" then
                ChangedSurnameFilter Nothing

            else
                ChangedSurnameFilter (Just text)
    in
    div []
        [ label [] [ text "Filter prefix:" ]
        , input [ onInput msg ] []
        ]


contactsListView : ContactDB -> SurnameFilter -> Html Msg
contactsListView db filter =
    let
        contactView row =
            option
                [ value (getIdAsString row)
                ]
                [ text (fullName row.data)
                ]

        fullName contact =
            Contact.surname contact
                ++ ", "
                ++ Contact.name contact

        options =
            db
                |> applySurnameFilter filter
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
        [ inputView ChangedName "Name" name
        , inputView ChangedSurname "Surname" surname
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
            Db.exists (Contact.contact model.name model.surname) model.contactDB

        canCreate =
            (model.name /= "")
                && (model.surname /= "")
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


getIdAsString : ContactRow -> String
getIdAsString =
    .id >> Row.idToString


applySurnameFilter : SurnameFilter -> ContactDB -> ContactDB
applySurnameFilter filter db =
    filter
        |> Maybe.map
            (\surname ->
                Db.filter (Contact.surname >> String.startsWith surname) db
            )
        |> Maybe.withDefault db
