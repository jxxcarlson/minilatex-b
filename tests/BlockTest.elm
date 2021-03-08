module BlockTest exposing (suite)

import Expect
import Parser.Block exposing (compile)
import Test exposing (describe, fuzz, test)


suite =
    describe "The Parser.Block module"
        [ test "compile for formula and multi-line text" <|
            \_ ->
                "Pythagoras said that\n\n$$a^2 + b^2 = c^2$$\n\n\none\ntwo\nthree"
                    |> compile 0
                    |> Expect.equal [ [ "Pythagoras said that" ], [ "$$a^2 + b^2 = c^2$$" ], [ "one", "two", "three" ] ]
        , test "environment" <|
            -- TODO: should we respect blank lines inside environments?
            \_ ->
                "\\begin{foo}\na\n\nb\nc\n\n\\end{foo}\n\n\none\ntwo\nthree"
                    |> compile 0
                    |> Expect.equal [ [ "\\begin{foo}", "a", "b", "c", "\\end{foo}" ], [ "one", "two", "three" ] ]
        ]
