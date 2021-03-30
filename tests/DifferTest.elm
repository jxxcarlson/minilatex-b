module DifferTest exposing (suite)

import Compiler.GenericDiffer exposing (DiffRecord, diff)
import Expect
import Parser.Block as Block
import Test exposing (describe, fuzz, test)


differTestFunction : String -> String -> String -> DiffRecord String -> Test.Test
differTestFunction desc text1 text2 expectedDiffRecord =
    let
        blocks1 : List String
        blocks1 =
            Block.compile 0 (String.lines text1)
                |> List.map (String.join "\n")

        blocks2 : List String
        blocks2 =
            Block.compile 0 (String.lines text2)
                |> List.map (String.join "\n")

        blockDiffRecord =
            diff blocks1 blocks2
    in
    test desc <|
        \_ -> Expect.equal blockDiffRecord expectedDiffRecord


suite0 =
    describe "Nothing"
        [ test "zero" <| \_ -> Expect.equal 2 2
        ]


suite =
    describe "Yada"
        [ describe "The GenericDiffer module"
            [ differTestFunction
                "Simple test"
                "a\n\nb\n\nc"
                "a\n\nx\n\nc"
                { commonInitialSegment = [ "a" ]
                , commonTerminalSegment = [ "c" ]
                , deltaInSource = [ "b" ]
                , deltaInTarget = [ "x" ]
                }
            , differTestFunction
                "Error in math display"
                "The teacher said:\n\n$$\nx^2\n$\n\nfoo bar baz"
                "The teacher said:\n\n$$\nx^2\n$$\n\nfoo bar baz"
                { commonInitialSegment = [ "The teacher said:" ]
                , commonTerminalSegment = [ "foo bar baz" ]
                , deltaInSource = [ "$$\nx^2\n$" ]

                -- TODO: the below is INCORRECT
                , deltaInTarget = [ "$$\nx^2\n$$" ]
                }
            , differTestFunction
                "Error in math display,but math display as separate paragraph"
                "The teacher said:\n\n$$\nx^2\n$\n\nfoo bar baz"
                "The teacher said:\n\n$$\nx^2\n$$\n\nfoo bar baz"
                { commonInitialSegment = [ "The teacher said:" ]
                , commonTerminalSegment = [ "foo bar baz" ]
                , deltaInSource = [ "$$\nx^2\n$" ]
                , deltaInTarget = [ "$$\nx^2\n$$" ]
                }
            , differTestFunction
                "Math display, one-character change in formula"
                "$$\nx^2\n$$"
                "$$\ny^2\n$$"
                { commonInitialSegment = []
                , commonTerminalSegment = []
                , deltaInSource = [ "$$\nx^2\n$$" ]
                , deltaInTarget = [ "$$\ny^2\n$$" ]
                }
            ]
        ]
