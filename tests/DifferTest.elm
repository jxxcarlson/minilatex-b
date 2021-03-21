module DifferTest exposing (..)

import Compiler.GenericDiffer exposing (DiffRecord, diff)
import Expect
import Test exposing (describe, fuzz, test)


suite =
    describe "The Parser.Block module"
        [ test "compile for formula and multi-line text" <|
            \_ ->
                ex1
                    |> String.lines
                    |> compile 0
        ]
