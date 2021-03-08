module BlockTest exposing (..)

import Expect
import Fuzz exposing (string)
import Parser.Block exposing (compile)
import Parser.Expression exposing (..)
import Parser.Parser exposing (..)
import Parser.TestHelper exposing (parseAndRecompose, roundTripCheck, squeezeSpace)
import Test exposing (describe, fuzz, test)


suite =
    describe "The Parser.Block module"
        [ test "parse pure text" <|
            \_ ->
                "Pythagoras said that\n\n$$a^2 + b^2 = c^2$$\n\n\none\ntwo\nthree"
                    |> compile 0
                    |> Expect.equal [ [ "Pythagoras said that" ], [ "$$a^2 + b^2 = c^2$$" ], [ "one", "two", "three" ] ]
        ]
