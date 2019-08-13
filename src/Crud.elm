module Crud exposing (main)

import Browser
import Crud.Contact as Contact exposing (Contact, Name, Surname)
import Crud.Db as Db exposing (DB)
import Crud.Style as Style
import Html exposing (Html, button, div, input, label, option, text)
import Html.Attributes exposing (disabled, multiple, style, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : Name
    , surname : Surname
    , surnameFilter : SurnameFilter
    , contactDB : ContactDB
    , selectedId : Maybe Contact.Id
    }


type alias ContactDB =
    DB Contact


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
    | SelectedContact (Maybe Contact.Id)


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
        ( newContact, db ) =
            Db.insert createContact model.contactDB

        createContact =
            String.fromInt >> Contact.contact model.name model.surname
    in
    { model
        | contactDB = db
        , selectedId = Just (Contact.id newContact)
    }


onClickedUpdate : Model -> Model
onClickedUpdate model =
    let
        updateContact id =
            Db.update (byId id) (Contact model.name model.surname id) model.contactDB
    in
    { model
        | contactDB =
            model.selectedId
                |> Maybe.map updateContact
                |> Maybe.withDefault model.contactDB
    }


onClickedDelete : Model -> Model
onClickedDelete model =
    { model
        | contactDB =
            model.selectedId
                |> Maybe.map (\id -> Db.delete (byId id) model.contactDB)
                |> Maybe.withDefault model.contactDB
        , selectedId = Nothing
    }


onSelectedContact : Maybe Contact.Id -> Model -> Model
onSelectedContact maybeId model =
    let
        maybeContact =
            maybeId
                |> Maybe.andThen (\id -> Db.get (byId id) model.contactDB)
    in
    case maybeContact of
        Nothing ->
            { model
                | selectedId = maybeId
                , name = ""
                , surname = ""
            }

        Just contact ->
            { model
                | selectedId = maybeId
                , name = Contact.name contact
                , surname = Contact.surname contact
            }



-- VIEW


view : Model -> Html Msg
view model =
    div [ Style.width "500px" ]
        [ div
            [ Style.width "50%"
            , marginBottom 0.5
            ]
            [ filterView ]
        , div
            [ Style.display "flex"
            , style "flex-direction" "row"
            , marginBottom 0.5
            ]
            [ div [ style "flex" "1" ] [ contactsListView model.contactDB model.surnameFilter ]
            , div [ style "flex" "1" ] [ newContactView model.name model.surname ]
            ]
        , controlsView model
        ]


filterView : Html Msg
filterView =
    div
        [ Style.display "flex" ]
        [ label
            [ Style.flex "1" ]
            [ text "Filter prefix:" ]
        , input
            [ Style.flex "2"
            , onInput (stringToMaybe >> ChangedSurnameFilter)
            ]
            []
        ]


contactsListView : ContactDB -> SurnameFilter -> Html Msg
contactsListView db filter =
    let
        options =
            db
                |> applySurnameFilter filter
                |> Db.map keyedOption
                |> Db.rows

        keyedOption contact =
            let
                id =
                    Contact.id contact
            in
            ( id
            , option
                [ value id ]
                [ text (fullName contact) ]
            )

        fullName contact =
            Contact.surname contact
                ++ ", "
                ++ Contact.name contact
    in
    keyedSelect
        [ multiple True
        , Style.width "100%"
        , Style.height "120px"
        , onInput (stringToMaybe >> SelectedContact)
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
        byFullName contact =
            (Contact.name contact == model.name)
                && (Contact.surname contact == model.surname)

        contactExists =
            Db.exists byFullName model.contactDB

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
        [ btnView ClickedCreate canCreate "Create"
        , btnView ClickedUpdate canUpdate "Update"
        , btnView ClickedDelete isSelected "Delete"
        ]


btnView : Msg -> Bool -> String -> Html Msg
btnView msg enabled text_ =
    button
        [ style "padding-right" "1em"
        , disabled (not enabled)
        , onClick msg
        ]
        [ text text_ ]



-- HELPERS


applySurnameFilter : SurnameFilter -> ContactDB -> ContactDB
applySurnameFilter filter db =
    filter
        |> Maybe.map
            (\surnamePrefix ->
                Db.filter (bySurnamePrefix surnamePrefix) db
            )
        |> Maybe.withDefault db


bySurnamePrefix : Contact.Surname -> Db.Query Contact
bySurnamePrefix prefix =
    .surname >> String.startsWith prefix


byId : Contact.Id -> Db.Query Contact
byId id =
    .id >> (==) id


keyedSelect : List (Html.Attribute msg) -> List ( String, Html msg ) -> Html msg
keyedSelect =
    Html.Keyed.node "select"


marginBottom : Float -> Html.Attribute msg
marginBottom em =
    style "margin-bottom" (String.fromFloat em ++ "em")


stringToMaybe : String -> Maybe String
stringToMaybe s =
    if s == "" then
        Nothing

    else
        Just s
