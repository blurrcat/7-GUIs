module Crud.Contact exposing
    ( Contact
    , Name
    , Surname
    , contact
    , name
    , surname
    )


type alias Name =
    String


type alias Surname =
    String


type alias Contact =
    { name : Name
    , surname : Surname
    }


contact : Name -> Surname -> Contact
contact name_ surname_ =
    { name = name_
    , surname = surname_
    }


name : Contact -> Name
name contact_ =
    contact_.name


surname : Contact -> Surname
surname contact_ =
    contact_.surname
