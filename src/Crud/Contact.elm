module Crud.Contact exposing (Contact, contact, id, name, surname)


type alias Name =
    String


type alias Surname =
    String


type ContactId
    = ContactId String


type alias Contact =
    { id : ContactId
    , name : Name
    , surname : Surname
    }


contact : String -> Name -> Surname -> Contact
contact stringId name_ surname_ =
    { id = ContactId stringId
    , name = name_
    , surname = surname_
    }


id : Contact -> String
id contact_ =
    let
        (ContactId stringId) =
            contact_.id
    in
    stringId


name : Contact -> Name
name contact_ =
    contact_.name


surname : Contact -> Surname
surname contact_ =
    contact_.surname
