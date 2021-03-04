module ParserTest exposing (suite)

import Expect
import Fuzz exposing (string)
import Parser.Advanced as Parser exposing (run)
import Parser.Expression exposing (..)
import Parser.Parser exposing (..)
import Parser.TestHelper exposing (parseAndRecompose, roundTripCheck, squeezeSpace)
import Test exposing (describe, fuzz, test)


suite =
    describe "The Parser module"
        [ describe "expressionList"
            -- Nest as many descriptions as you like.
            [ test "parse pure text" <|
                \_ ->
                    "a b c"
                        |> run (expression 0)
                        |> Expect.equal (Ok (Text "a b c" { chunkOffset = 0, content = "a b c", length = 5, offset = 0 }))
            , test "parse inline math" <|
                \_ ->
                    "$x^2$"
                        |> run (expression 0)
                        |> Expect.equal (Ok (InlineMath "x^2" { chunkOffset = 0, content = "x^2", length = 5, offset = 0 }))
            , test "parse text and math" <|
                \_ ->
                    "a quadratic: $x^2$"
                        |> run (expressionList 0)
                        |> Expect.equal (Ok [ Text "a quadratic: " { chunkOffset = 0, content = "a quadratic: ", length = 13, offset = 0 }, InlineMath "x^2" { chunkOffset = 0, content = "x^2", length = 18, offset = 0 } ])
            , test "parse macro" <|
                \_ ->
                    "\\strong{stuff}"
                        |> run (expression 0)
                        |> Expect.equal (Ok (Macro "strong" Nothing [ Text "stuff" { chunkOffset = 0, content = "stuff", length = 5, offset = 0 } ] { chunkOffset = 0, content = "\\strong{stuff}", length = 14, offset = 0 }))

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]
