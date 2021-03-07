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
                        |> run (expression 0 0)
                        |> Expect.equal (Ok (Text "a b c" { generation = 0, blockOffset = 0, content = "a b c", length = 5, offset = 0 }))
            , test "parse inline math" <|
                \_ ->
                    "$x^2$"
                        |> run (expression 0 0)
                        |> Expect.equal (Ok (InlineMath "x^2" { generation = 0, blockOffset = 0, content = "x^2", length = 5, offset = 0 }))
            , test "parse inline math with error" <|
                \_ ->
                    "$x^2"
                        |> run (expression 0 0)
                        |> Expect.equal (Err [ { col = 5, contextStack = [ { col = 1, context = InlineMathContext, row = 1 } ], problem = ExpectingTrailingDollarSign, row = 1 } ])
            , test "parse text and math" <|
                \_ ->
                    "a quadratic: $x^2$"
                        |> run (expressionList 0 0)
                        |> Expect.equal (Ok [ Text "a quadratic: " { generation = 0, blockOffset = 0, content = "a quadratic: ", length = 13, offset = 0 }, InlineMath "x^2" { generation = 0, blockOffset = 0, content = "x^2", length = 18, offset = 0 } ])
            , test "parse macro" <|
                \_ ->
                    "\\strong{stuff}"
                        -- The value 13 should be 5, but "interior" source maps are irrelevant
                        |> run (expression 0 0)
                        |> Expect.equal (Ok (Macro "strong" Nothing [ Text "stuff" { generation = 0, blockOffset = 0, content = "stuff", length = 13, offset = 0 } ] { generation = 0, blockOffset = 0, content = "\\strong{stuff}", length = 14, offset = 0 }))
            , test "parse macro with error" <|
                \_ ->
                    "\\strong{stuff"
                        |> run (expression 0 0)
                        |> Expect.equal (Err [ { col = 14, contextStack = [ { col = 8, context = ArgContext, row = 1 } ], problem = ExpectingRightBrace, row = 1 } ])
            , test "parse arg" <|
                \_ ->
                    "{123}"
                        -- The value 4 should be 5 (count the braces}, but "interior" source maps are irrelevant
                        |> run (arg 0 0)
                        |> Expect.equal (Ok (Text "123" { generation = 0, blockOffset = 0, content = "123", length = 4, offset = 0 }))
            , test "parse macro name" <|
                \_ ->
                    "\\strong"
                        |> run (macroName 0 0)
                        |> Expect.equal (Ok ( "strong", { generation = 0, blockOffset = 0, content = "strong", length = 7, offset = 0 } ))
            , test "parse invalid macro name" <|
                \_ ->
                    "\\begin"
                        |> run (macroName 0 0)
                        |> Expect.equal (Err [ { col = 1, contextStack = [], problem = RejectMacroReservedWord, row = 1 } ])

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]
