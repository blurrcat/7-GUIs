module Crud.Contact exposing
    ( Contact
    , Id
    , Name
    , Surname
    , contact
    , id
    , name
    , surname
    )


type alias Id =
    String


type alias Name =
    String


type alias Surname =
    String


type alias Contact =
    { name : Name
    , surname : Surname
    , id : Id
    }


contact : Name -> Surname -> Id -> Contact
contact name_ surname_ id_ =
    { name = name_
    , surname = surname_
    , id = id_
    }


id : Contact -> Id
id =
    .id


name : Contact -> Name
name =
    .name


surname : Contact -> Surname
surname =
    .surname
