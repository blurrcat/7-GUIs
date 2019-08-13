module Crud.Db exposing
    ( DB
    , Query
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


type alias DB a =
    { seq : Int
    , rows : List a
    }


type alias Query a =
    a -> Bool


db : DB a
db =
    { seq = 1, rows = [] }


rows : DB a -> List a
rows =
    .rows


insert : (Int -> a) -> DB a -> ( a, DB a )
insert createRow db_ =
    let
        newRow =
            createRow db_.seq
    in
    ( newRow
    , { db
        | seq = db_.seq + 1
        , rows = newRow :: db_.rows
      }
    )


map : (a -> b) -> DB a -> DB b
map fn db_ =
    { db
        | rows = List.map fn db_.rows
    }


filter : Query a -> DB a -> DB a
filter query db_ =
    { db_
        | rows = List.filter query db_.rows
    }


delete : Query a -> DB a -> DB a
delete query =
    filter (not << query)


exists : Query a -> DB a -> Bool
exists query =
    .rows
        >> List.any query


get : Query a -> DB a -> Maybe a
get query =
    .rows
        >> List.filter query
        >> List.head


update : Query a -> a -> DB a -> DB a
update query data db_ =
    let
        update_ row =
            if query row then
                data

            else
                row
    in
    { db_ | rows = List.map update_ db_.rows }
