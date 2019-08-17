module CircleDrawer.Point exposing (Point, decoder, toString)

import Json.Decode as D


type alias Point =
    { x : Float
    , y : Float
    }


decoder : D.Decoder Point
decoder =
    D.map2 Point
        (D.field "x" D.float)
        (D.field "y" D.float)


toString : Point -> String
toString { x, y } =
    "(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")"
