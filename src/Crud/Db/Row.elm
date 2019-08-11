module Crud.Db.Row exposing (Id, Row, id, idToString, map, match, row)


type Id
    = Id String


type alias Row a =
    { id : Id
    , data : a
    }


row : String -> a -> Row a
row id_ data =
    { id = Id id_, data = data }


map : (a -> b) -> Row a -> Row b
map fn r =
    { id = r.id, data = fn r.data }


match : (a -> Bool) -> Row a -> Bool
match fn r =
    fn r.data


id : String -> Id
id stringId =
    Id stringId


idToString : Id -> String
idToString (Id s) =
    s
