port module CircleDrawer.Ports exposing (Point, mouseDownOn)

import Json.Decode as D
import Json.Encode as E


port onMouseDownOnSvg : (E.Value -> msg) -> Sub msg


type alias Point =
    ( Float, Float )


mouseDownOn : (Maybe Point -> msg) -> Sub msg
mouseDownOn toMsg =
    onMouseDownOnSvg (decodePointValue >> toMsg)


point : Float -> Float -> Point
point =
    Tuple.pair


pointDecoder : D.Decoder Point
pointDecoder =
    D.map2 point
        (D.field "x" D.float)
        (D.field "y" D.float)


decodePointValue : E.Value -> Maybe Point
decodePointValue value =
    D.decodeValue pointDecoder value
        |> Result.toMaybe
