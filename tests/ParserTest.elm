module ParserTest exposing (exItemized, suite)

import Expect
import Fuzz exposing (string)
import Parser.Advanced as Parser exposing (run)
import Parser.Expression exposing (..)
import Parser.Parser exposing (..)
import Parser.TestHelper exposing (parseAndRecompose, roundTripCheck, squeezeSpace)
import Test exposing (describe, fuzz, test)


exItemized =
    """
\\begin{itemize}

\\item Eggs

\\item Milk

\\item Butter

\\end{itemize}
"""


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
            , test "environment containing display math" <|
                \_ ->
                    "\\begin{foo}\n$$\na^2 + b^2\n$$\n\\end{foo}"
                        |> run (expression 0 0)
                        |> Expect.equal (Ok (Environment "foo" [] (LXList [ DisplayMath "\na^2 + b^2\n" { blockOffset = 0, content = "\na^2 + b^2\n", generation = 0, length = 27, offset = 0 } ]) { blockOffset = 0, content = "\\begin{foo}\n$$\na^2 + b^2\n$$\n\\end{foo}", generation = 0, length = 37, offset = 0 }))
            , test "item" <|
                \_ -> "\\item foo" |> run (item 0 0) |> Expect.equal (Ok (Item 1 (LXList [ Text "foo" { blockOffset = 0, content = "foo", generation = 0, length = 3, offset = 6 } ]) { blockOffset = 0, content = "\\item foo", generation = 0, length = 9, offset = 0 }))
            , test "itemize" <|
                \_ -> "\\begin{itemize}\n\\item foo\n\\end{itemize}" |> run (expression 0 0) |> Expect.equal (Ok (Environment "itemize" [] (LXList [ Item 1 (LXList [ Text "foo\n" { blockOffset = 0, content = "foo\n", generation = 0, length = 4, offset = 22 } ]) { blockOffset = 0, content = "\\begin{itemize}\n\\item foo\n\\end{itemize}", generation = 0, length = 10, offset = 16 } ]) { blockOffset = 0, content = "\\begin{itemize}\n\\item foo\n\\end{itemize}", generation = 0, length = 39, offset = 0 }))

            --, test "tableCell" <|
            --    \_ -> "\\strong{stuff}" |> run (tableCell 0 0) |> Result.map Parser.Expression.strip |> Expect.equal (Ok (LXList_ [ Macro_ "strong" Nothing [ Text_ "stuff" ] ]))
            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]
