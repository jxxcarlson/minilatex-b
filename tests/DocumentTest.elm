module DocumentTest exposing (suite)

import Expect
import Parser.Block as Block
import Parser.Document exposing (State, process)
import Parser.Expression exposing (BareExpression(..), Expression(..))
import Test exposing (describe, fuzz, test)


documentInitTestFunction : String -> String -> List (List BareExpression) -> Test.Test
documentInitTestFunction desc text expectedParsed =
    let
        state =
            process 0 (String.lines text)

        parsed : List (List Parser.Expression.BareExpression)
        parsed =
            state.output |> List.map .parsed |> Parser.Expression.stripList2
    in
    test desc <|
        \_ -> Expect.equal parsed expectedParsed


suite =
    describe "module Parser.Document"
        [ describe "Document.init, happy path"
            [ documentInitTestFunction
                "One line of text"
                "hello"
                [ [ Text_ "hello" ] ]
            , documentInitTestFunction
                "Several lines of text"
                "one\ntwo\nthree"
                [ [ Text_ "one\ntwo\nthree" ] ]
            , documentInitTestFunction
                "Macro and inline math"
                "\\strong{Example:} $a^2 + b^2 = c^2$"
                [ [ Macro_ "strong" Nothing [ Text_ "Example:" ], Text_ " ", InlineMath_ "a^2 + b^2 = c^2" ] ]
            , documentInitTestFunction
                "Macro and display math"
                "\\strong{Example:}\n\n$$\na^2 + b^2 = c^2$$"
                [ [ Macro_ "strong" Nothing [ Text_ "Example:" ] ], [ DisplayMath_ "\na^2 + b^2 = c^2" ] ]
            , documentInitTestFunction
                "Macro and display math with trailing text"
                "\\strong{Example:}\n\n$$\na^2 + b^2 = c^2$$\n\nWow!"
                [ [ Macro_ "strong" Nothing [ Text_ "Example:" ] ], [ DisplayMath_ "\na^2 + b^2 = c^2" ], [ Text_ "Wow!" ] ]
            , documentInitTestFunction
                "Environment with trailing text"
                "Start\n\n\\begin{theorem}Many primes!\n\\end{theorem}\n\nWow!"
                [ [ Text_ "Start" ], [ Environment_ "theorem" [] (LXList_ [ Text_ "Many primes!\n" ]) ], [ Text_ "Wow!" ] ]
            , documentInitTestFunction
                "Nested environment"
                "Start\n\n\\begin{indent}\n\n\\begin{theorem}Many primes!\n\\end{theorem}\n\n\\end{indent}\n\nWow!"
                [ [ Text_ "Start" ], [ Environment_ "indent" [] (LXList_ [ Environment_ "theorem" [] (LXList_ [ Text_ "Many primes!\n" ]), Text_ "\n" ]) ], [ Text_ "Wow!" ] ]
            , documentInitTestFunction
                "List"
                "\\begin{itemize}\n\\item One\\item Two\n\\end{itemize}\n\n"
                [ [ Environment_ "itemize" [] (LXList_ [ Item_ 1 (LXList_ [ Text_ "One" ]), Item_ 1 (LXList_ [ Text_ "Two\n" ]) ]) ] ]
            , documentInitTestFunction
                "Table"
                "\\begin{tabular}{l l}A & 1 \\\\\nB & 2\n\\end{tabular}\n\n"
                [ [ Environment_ "tabular"
                        [ Text_ "l l" ]
                        (LXList_
                            [ LXList_ [ LXList_ [ Text_ "A " ], LXList_ [ Text_ "1 " ] ]
                            , LXList_ [ LXList_ [ Text_ "B " ], LXList_ [ Text_ "2" ] ]
                            ]
                        )
                  ]
                ]
            , documentInitTestFunction
                "Text macro definition"
                "\\begin{textmacro}\n\\newcommand{\\foo}{FOO}\n\\end{textmacro}\n"
                [ [ Environment_ "textmacro" [] (Text_ "\\newcommand{\\foo}{FOO}") ] ]
            , documentInitTestFunction
                "Math macro definition"
                "\\begin{mathmacro}\n\\newcommand{\\bt}[1]{\\bf{#1}}\n\\end{mathmacro}\n"
                [ [ Environment_ "mathmacro" [] (Text_ "\\newcommand{\\bt}[1]{\\bf{#1}}") ] ]
            ]
            ,  describe "Document.init, error path" [

                   documentInitTestFunction
                              -- This example shows (1) good error message (2) how errors in one block do not propagate to subsequent blocks
                              "Macro with error"
                              "\\strong{Example: $a^2 + b^2 = c^2$\n\nmore stuff ..."
                              [[LXList_ [Macro_ "red" Nothing [Text_ "!! missing right brace in \\"]]
                                ,Macro_ "blue" Nothing [Text_ "strong{Example: "]
                                ,Macro_ "blue" Nothing [InlineMath_ "a^2 + b^2 = c^2"]]
                                ,[Text_ "more stuff ..."]]

                   , documentInitTestFunction
                              -- This example shows (1) good error message (2) how errors in one block do not propagate to subsequent blocks
                              "Error in inline math (unclosed $)"
                              "Pythagoras: $a^2 + b^2 = c^2 (wow!) \n\nmore stuff ..."
                              [[Text_ "Pythagoras: "
                                ,LXList_ [Macro_ "red" Nothing [Text_ "⚠ unmatched $ in "]]
                                ,Macro_ "blue" Nothing [Text_ "a^2 + b^2 = c^2 (wow!) "]]
                                ,[Text_ "more stuff ..."]]

                   , documentInitTestFunction
                              -- This example shows (1) good error message (2) how errors in one block do not propagate to subsequent blocks
                              "Error in theorem environment (unmatched begin-end)"
                              "\\begin{theorem}\nSo many primes!\n\\end{theore\n\nmore stuff ..."
                              [[Environment_ "theorem" [] (LXList_ [Text_ "So many primes!\n"])
                                 ,Macro_ "red" Nothing [Text_ "^^ I fixed the theorem environment for you (unmatched begin-end pair); please correct it."]
                                 ,Text_ "\n",Macro_ "bigskip" Nothing []]]
               ]
            ]

