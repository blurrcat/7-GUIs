module Crud.Db exposing
    ( DB
    , db
    , delete
    , exists
    , filter
    , get
    , insert
    , map
    , rows
    , update
    )

import Crud.Db.Row as Row exposing (Row)


type alias DB a =
    { seq : Int
    , rows : List (Row a)
    }


db : DB a
db =
    { seq = 1, rows = [] }


insert : a -> DB a -> DB a
insert data db_ =
    let
        doesExist =
            exists data db_
    in
    if doesExist then
        db_

    else
        { db
            | seq = db_.seq + 1
            , rows = List.append [ Row.row (String.fromInt db_.seq) data ] db_.rows
        }


map : (a -> b) -> DB a -> DB b
map fn db_ =
    { db
        | rows = List.map (Row.map fn) db_.rows
    }


filter : (a -> Bool) -> DB a -> DB a
filter match db_ =
    { db_
        | rows = List.filter (Row.match match) db_.rows
    }


rows : DB a -> List (Row a)
rows db_ =
    db_.rows


delete : Row.Id -> DB a -> DB a
delete id db_ =
    { db
        | rows = List.filter (matchById id) db_.rows
    }


exists : a -> DB a -> Bool
exists data db_ =
    db_.rows
        |> List.map .data
        |> List.member data


get : Row.Id -> DB a -> Maybe (Row a)
get id db_ =
    db_.rows
        |> List.filter (matchById id)
        |> List.head


update : Row.Id -> a -> DB a -> DB a
update id data db_ =
    let
        update_ row =
            if row.id == id then
                Row id data

            else
                row
    in
    { db_
        | rows =
            db_.rows
                |> List.map update_
    }



-- HELPERS


matchById : Row.Id -> (Row a -> Bool)
matchById id =
    .id >> (==) id
