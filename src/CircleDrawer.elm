module CircleDrawer exposing (Model, Msg, init, subscriptions, update, view)

import CircleDrawer.Circle as Circle exposing (Circle)
import CircleDrawer.History as History exposing (History)
import CircleDrawer.Point as Point exposing (Point)
import CircleDrawer.Ports as Ports
    exposing
        ( CircleClick
        , clickedPoints
        , selectedCircles
        )
import Html exposing (Html, button, div, input, text)
import Html.Attributes as HA exposing (disabled, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Svg exposing (Svg, circle)
import Svg.Attributes as SA
import Svg.Keyed



-- MODEL


type alias Model =
    { circlesHistory : CirclesHistory
    , selectedCircle : SelectedCircle
    , showEditRadius : Bool
    }


type alias CirclesHistory =
    History (List Circle)


type alias SelectedCircle =
    Maybe Circle


init : () -> ( Model, Cmd Msg )
init _ =
    ( { circlesHistory = History.history []
      , selectedCircle = Nothing
      , showEditRadius = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedPoint (Maybe Point)
    | SelectedCircle (Maybe CircleClick)
    | ChangedRadius (Maybe Float)
    | SavedRadius
    | Undo
    | Redo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPoint maybePoint ->
            ( onClickedCircle maybePoint model, Cmd.none )

        SelectedCircle maybeCircleClick ->
            ( onSelectedCircle maybeCircleClick model, Cmd.none )

        ChangedRadius maybeRadius ->
            ( onChangedRadius maybeRadius model, Cmd.none )

        SavedRadius ->
            ( onSavedRadius model, Cmd.none )

        Undo ->
            ( onUndo model, Cmd.none )

        Redo ->
            ( onRedo model, Cmd.none )


onClickedCircle : Maybe Point -> Model -> Model
onClickedCircle maybePoint model =
    let
        id =
            History.current model.circlesHistory
                |> List.length
                |> String.fromInt

        createCircle circles =
            maybePoint
                |> Maybe.map (\p -> circles ++ [ defaultCircle p id ])
                |> Maybe.withDefault circles
    in
    { model
        | circlesHistory = History.do createCircle model.circlesHistory
        , selectedCircle = Nothing
    }


onSelectedCircle : Maybe CircleClick -> Model -> Model
onSelectedCircle maybeCircleClick model =
    case maybeCircleClick of
        Just click ->
            let
                selectedCircle =
                    model.circlesHistory
                        |> History.current
                        |> List.filter (\c -> Circle.id c == click.id)
                        |> List.head
            in
            { model
                | selectedCircle = selectedCircle
                , showEditRadius =
                    (click.click == Ports.RightClick)
                        && (selectedCircle /= Nothing)
            }

        Nothing ->
            model


onChangedRadius : Maybe Float -> Model -> Model
onChangedRadius maybeRadius model =
    case maybeRadius of
        Just radius ->
            { model
                | selectedCircle =
                    model.selectedCircle
                        |> Maybe.map (circleWithNewRadius radius)
            }

        Nothing ->
            model


onSavedRadius : Model -> Model
onSavedRadius model =
    let
        updateCircle selectedCircle circle =
            if Circle.id selectedCircle == Circle.id circle then
                circleWithNewRadius (Circle.radius selectedCircle) circle

            else
                circle

        updateCircles selectedCircle =
            List.map (updateCircle selectedCircle)

        circlesHistory =
            case model.selectedCircle of
                Just selectedCircle ->
                    model.circlesHistory
                        |> History.do (updateCircles selectedCircle)

                Nothing ->
                    model.circlesHistory
    in
    { model
        | circlesHistory = circlesHistory
        , showEditRadius = False
        , selectedCircle = Nothing
    }


onUndo : Model -> Model
onUndo model =
    { model | circlesHistory = History.undo model.circlesHistory }


onRedo : Model -> Model
onRedo model =
    { model | circlesHistory = History.redo model.circlesHistory }


minRadius : Float
minRadius =
    2


maxRadius : Float
maxRadius =
    10


defaultCircle : Point -> String -> Circle
defaultCircle =
    Circle.circle 5


circleWithNewRadius : Float -> Circle -> Circle
circleWithNewRadius radius circle =
    Circle.circle radius (Circle.center circle) (Circle.id circle)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ clickedPoints ClickedPoint
        , selectedCircles SelectedCircle
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        circles =
            History.current model.circlesHistory
    in
    div
        [ style "width" "400px"
        , style "height" "400px"
        ]
        [ buttonsView model.circlesHistory
        , canvasView model.selectedCircle circles
        , editCircleRadiusView model.showEditRadius model.selectedCircle
        ]


buttonsView : CirclesHistory -> Html Msg
buttonsView history =
    div
        [ style "text-align" "center"
        , style "padding" "0.5em"
        ]
        [ button
            [ onClick Undo
            , disabled (not (History.canUndo history))
            ]
            [ text "Undo" ]
        , button
            [ onClick Redo
            , disabled (not (History.canRedo history))
            ]
            [ text "Redo" ]
        ]


editCircleRadiusView : Bool -> SelectedCircle -> Html Msg
editCircleRadiusView shouldShow selectedCircle =
    div [ style "height" "3em" ]
        [ if shouldShow then
            selectedCircle
                |> Maybe.map editRadiusView
                |> Maybe.withDefault emptyView

          else
            emptyView
        ]


editRadiusView : Circle -> Html Msg
editRadiusView selectedCircle =
    let
        coordinate =
            selectedCircle
                |> Circle.center
                |> Point.toString
    in
    div
        []
        [ div [ style "text-align" "center" ]
            [ text ("Adjust diameter of circle at " ++ coordinate)
            ]
        , div
            [ style "display" "flex"
            , style "flex-direction" "row"
            , style "justify-content" "space-evenly"
            ]
            [ div []
                [ input
                    [ type_ "range"
                    , value (String.fromFloat (Circle.radius selectedCircle))
                    , onInput (String.toFloat >> ChangedRadius)
                    , HA.max (String.fromFloat maxRadius)
                    , HA.min (String.fromFloat minRadius)
                    , HA.step "any"
                    ]
                    []
                ]
            , div []
                [ button
                    [ onClick SavedRadius
                    , style "float" "right"
                    ]
                    [ text "Save" ]
                ]
            ]
        ]


emptyView : Html msg
emptyView =
    text ""


canvasView : SelectedCircle -> List Circle -> Html msg
canvasView selectedCircle circles =
    Svg.Keyed.node "svg"
        [ SA.viewBox "0 0 100 100"
        , SA.width "100%"
        , SA.height "100%"
        , style "border" "solid 1px #333333"
        ]
        (List.map (circleView selectedCircle) circles)


circleView : SelectedCircle -> Circle -> ( String, Svg msg )
circleView selectedCircle circle =
    let
        center =
            Circle.center circle

        id =
            Circle.id circle

        selected =
            selectedCircle
                |> Maybe.map (Circle.id >> (==) id)
                |> Maybe.withDefault False

        radius =
            if selected then
                selectedCircle
                    |> Maybe.map Circle.radius
                    |> Maybe.withDefault (Circle.radius circle)

            else
                Circle.radius circle

        fill =
            if selected then
                "grey"

            else
                "white"
    in
    ( id
    , Svg.circle
        [ SA.cx (String.fromFloat center.x)
        , SA.cy (String.fromFloat center.y)
        , SA.r (String.fromFloat radius)
        , SA.fill fill
        , SA.stroke "black"
        , SA.strokeWidth "0.2"
        , SA.pointerEvents "visiblePoint"
        , SA.id id
        ]
        []
    )
