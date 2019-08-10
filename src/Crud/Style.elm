module Crud.Style exposing (display, width, widthEm)

import Html exposing (Attribute)
import Html.Attributes exposing (style)


width : String -> Attribute msg
width =
    style "width"


display : String -> Attribute msg
display =
    style "display"


widthEm : Float -> Attribute msg
widthEm em =
    width (String.fromFloat em ++ "em")
