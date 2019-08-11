module Crud.Contact exposing
    ( Contact
    , Name
    , Surname
    , contact
    , getName
    , getSurname
    , name
    , nameToString
    , surname
    , surnameToString
    )


type Name
    = Name String


type Surname
    = Surname String


type alias Contact =
    { name : Name
    , surname : Surname
    }


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


contact : Name -> Surname -> Contact
contact name_ surname_ =
    { name = name_
    , surname = surname_
    }


getName : Contact -> Name
getName contact_ =
    contact_.name


getSurname : Contact -> Surname
getSurname contact_ =
    contact_.surname
