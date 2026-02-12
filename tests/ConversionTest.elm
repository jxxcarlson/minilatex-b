module ConversionTest exposing (suite)

import Expect
import MiniLaTeX
import Scripta.ToString
import Test exposing (describe, test)


suite =
    describe "Scripta.FromLaTeX.convertFromString"
        [ test "Plain text is converted correctly (without change)" <|
            \_ ->
                let
                    input = "This is a test"
                    output = Scripta.ToString.fromString input
                in
                Expect.equal input output
        ]
