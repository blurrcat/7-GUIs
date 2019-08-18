module CircleDrawer.History exposing
    ( History
    , canRedo
    , canUndo
    , current
    , do
    , history
    , redo
    , undo
    )


type History a
    = History
        { undos : List a
        , current : a
        , redos : List a
        }


history : a -> History a
history a =
    History
        { undos = []
        , current = a
        , redos = []
        }


current : History a -> a
current (History hist) =
    hist.current


do : (a -> a) -> History a -> History a
do action (History hist) =
    let
        next =
            action hist.current
    in
    History
        { undos = hist.current :: hist.undos
        , current = next
        , redos = []
        }


undo : History a -> History a
undo (History hist) =
    History
        (case hist.undos of
            last :: rest ->
                { undos = rest
                , current = last
                , redos = hist.current :: hist.redos
                }

            [] ->
                hist
        )


redo : History a -> History a
redo (History hist) =
    History
        (case hist.redos of
            last :: rest ->
                { undos = hist.current :: hist.undos
                , current = last
                , redos = rest
                }

            [] ->
                hist
        )


canUndo : History a -> Bool
canUndo (History { undos }) =
    not (List.isEmpty undos)


canRedo : History a -> Bool
canRedo (History { redos }) =
    not (List.isEmpty redos)
