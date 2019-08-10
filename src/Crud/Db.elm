module Crud.Db exposing (DB, db, delete, filter, insert, map, rows)


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
filter match db_ =
    { db
        | rows = List.filter match db_.rows
    }


rows : DB a -> List a
rows db_ =
    db_.rows


delete : (a -> Bool) -> DB a -> DB a
delete match db_ =
    { db
        | rows = List.filter (not << match) db_.rows
    }
