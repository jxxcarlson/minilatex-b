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
            , test "simple environments" <|
                \_ ->
                    "\\begin{foo}\nxyz\n\\end{foo}"
                        |> run (expression 0 0)
                        |> Expect.equal (Ok (Environment "foo" [] (LXList [ Text "xyz\n" { blockOffset = 0, content = "xyz\n", generation = 0, length = 4, offset = 12 } ]) { blockOffset = 0, content = "\\begin{foo}\nxyz\n\\end{foo}", generation = 0, length = 25, offset = 0 }))
            , test "nested environments" <|
                \_ ->
                    "\\begin{foo}\n\\begin{bar}\nxyz\n\\end{bar}\n\\end{foo}"
                        |> run (expression 0 0)
                        |> Expect.equal (Ok (Environment "foo" [] (LXList [ Environment "bar" [] (LXList [ Text "xyz\n" { blockOffset = 0, content = "xyz\n", generation = 0, length = 4, offset = 24 } ]) { blockOffset = 0, content = "\\begin{foo}\n\\begin{bar}\nxyz\n\\end{bar}\n\\end{foo}", generation = 0, length = 25, offset = 12 }, Text "\n" { blockOffset = 0, content = "\n", generation = 0, length = 1, offset = 37 } ]) { blockOffset = 0, content = "\\begin{foo}\n\\begin{bar}\nxyz\n\\end{bar}\n\\end{foo}", generation = 0, length = 47, offset = 0 }))

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]
