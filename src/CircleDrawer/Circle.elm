module CircleDrawer.Circle exposing (Circle, center, circle, id, radius)

import CircleDrawer.Point exposing (Point)


type Circle
    = Circle
        { radius : Float
        , center : Point
        , id : String
        }


circle : Float -> Point -> String -> Circle
circle r c id_ =
    Circle { radius = r, center = c, id = id_ }


radius : Circle -> Float
radius (Circle c) =
    c.radius


center : Circle -> Point
center (Circle c) =
    c.center


id : Circle -> String
id (Circle c) =
    c.id
