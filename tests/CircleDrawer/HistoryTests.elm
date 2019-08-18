module CircleDrawer.HistoryTests exposing (suite)

import CircleDrawer.History as History
import Expect
import Fuzz exposing (int)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "History"
        [ describe "current"
            [ fuzz int "should return the current state" <|
                \count ->
                    History.history count
                        |> History.current
                        |> Expect.equal count
            ]
        , describe "do"
            [ fuzz int "should act on the current state" <|
                \count ->
                    History.history count
                        |> History.do addOne
                        |> History.current
                        |> Expect.equal (addOne count)
            , fuzz int "should clear the redo history" <|
                \count ->
                    History.history count
                        |> History.do addOne
                        |> History.undo
                        |> History.do addOne
                        |> History.redo
                        |> History.current
                        |> Expect.equal (addOne count)
            ]
        , describe "undo"
            [ fuzz int "should do nothing if there's nothing to undo" <|
                \count ->
                    History.history count
                        |> History.undo
                        |> History.current
                        |> Expect.equal count
            , fuzz int "should undo the last action if there is something to undo" <|
                \count ->
                    History.history count
                        |> History.do addOne
                        |> History.undo
                        |> History.current
                        |> Expect.equal count
            ]
        , describe "canUndo"
            [ fuzz int "should be false if there's no undo history" <|
                \count ->
                    History.history count
                        |> History.canUndo
                        |> Expect.equal False
            , fuzz int "should be true if there's at least 1 undo action" <|
                \count ->
                    History.history count
                        |> History.do addOne
                        |> History.canUndo
                        |> Expect.equal True
            ]
        , describe "redo"
            [ fuzz int "should do nothing if there's nothing to redo" <|
                \count ->
                    History.history count
                        |> History.redo
                        |> History.current
                        |> Expect.equal count
            , fuzz int "should redo the last action if there is something to redo" <|
                \count ->
                    History.history count
                        |> History.do addOne
                        |> History.undo
                        |> History.redo
                        |> History.current
                        |> Expect.equal (addOne count)
            ]
        , describe "canRedo"
            [ fuzz int "should be false if there's no redo history" <|
                \count ->
                    History.history count
                        |> History.canRedo
                        |> Expect.equal False
            , fuzz int "should be true if there's at least 1 redo action" <|
                \count ->
                    History.history count
                        |> History.do addOne
                        |> History.undo
                        |> History.canRedo
                        |> Expect.equal True
            ]
        ]


addOne : Int -> Int
addOne =
    (+) 1
