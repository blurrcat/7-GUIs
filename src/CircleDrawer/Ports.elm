port module CircleDrawer.Ports exposing
    ( CircleClick
    , Click(..)
    , clickedPoints
    , selectedCircles
    )

import CircleDrawer.Point as Point exposing (Point)
import Json.Decode as D
import Json.Encode as E


port clickedPoint : (E.Value -> msg) -> Sub msg


port selectedCircle : (E.Value -> msg) -> Sub msg


type Click
    = LeftClick
    | RightClick


clickDecoder : D.Decoder Click
clickDecoder =
    let
        mapClick click =
            case click of
                "left" ->
                    D.succeed LeftClick

                "right" ->
                    D.succeed RightClick

                _ ->
                    D.fail "expected one of ['left', 'right']"
    in
    D.string
        |> D.andThen mapClick


type alias CircleClick =
    { id : String
    , click : Click
    }


circleClickDecoder : D.Decoder CircleClick
circleClickDecoder =
    D.map2 CircleClick
        (D.field "id" D.string)
        (D.field "click" clickDecoder)


clickedPoints : (Maybe Point -> msg) -> Sub msg
clickedPoints toMsg =
    clickedPoint (decodePointValue >> toMsg)


selectedCircles : (Maybe CircleClick -> msg) -> Sub msg
selectedCircles toMsg =
    selectedCircle (decodeSelectedCircle >> toMsg)


decodeValue : D.Decoder a -> E.Value -> Maybe a
decodeValue decoder value =
    D.decodeValue decoder value
        |> Result.toMaybe


decodePointValue : E.Value -> Maybe Point
decodePointValue =
    decodeValue Point.decoder


decodeSelectedCircle : E.Value -> Maybe CircleClick
decodeSelectedCircle =
    decodeValue circleClickDecoder
