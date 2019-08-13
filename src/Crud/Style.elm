module Crud.Style exposing (display, flex, height, width)

import Html exposing (Attribute)
import Html.Attributes exposing (style)


width : String -> Attribute msg
width =
    style "width"


height : String -> Attribute msg
height =
    style "height"


display : String -> Attribute msg
display =
    style "display"


flex : String -> Attribute msg
flex =
    style "flex"
