module Crud.Db exposing (DB, db, filter, insert, map, rows)


type alias DB a =
    { seq : Int
    , rows : List a
    }


db : DB a
db =
    { seq = 1, rows = [] }


insert : (String -> a) -> DB a -> DB a
insert newRow db_ =
    { db
        | seq = db_.seq + 1
        , rows = newRow (String.fromInt db_.seq) :: db_.rows
    }


map : (a -> b) -> DB a -> DB b
map fn db_ =
    { db
        | rows = List.map fn db_.rows
    }


filter : (a -> Bool) -> DB a -> DB a
filter f db_ =
    { db
        | rows = List.filter f db_.rows
    }


rows : DB a -> List a
rows db_ =
    db_.rows
