module Crud.Contact exposing
    ( Contact
    , Id
    , Name
    , Surname
    , contact
    , getId
    , getName
    , getSurname
    , id
    , idToString
    , name
    , nameToString
    , surname
    , surnameToString
    )


type Name
    = Name String


type Surname
    = Surname String


type Id
    = Id String


type alias Contact =
    { id : Id
    , name : Name
    , surname : Surname
    }


id : String -> Id
id text =
    Id text


idToString : Id -> String
idToString (Id text) =
    text


name : String -> Name
name text =
    Name text


nameToString : Name -> String
nameToString (Name text) =
    text


surname : String -> Surname
surname text =
    Surname text


surnameToString : Surname -> String
surnameToString (Surname text) =
    text


contact : Name -> Surname -> Id -> Contact
contact name_ surname_ id_ =
    { id = id_
    , name = name_
    , surname = surname_
    }


getId : Contact -> String
getId contact_ =
    idToString contact_.id


getName : Contact -> String
getName contact_ =
    nameToString contact_.name


getSurname : Contact -> String
getSurname contact_ =
    surnameToString contact_.surname
