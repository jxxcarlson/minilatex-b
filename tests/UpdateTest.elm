module UpdateTest exposing (suite)

import Expect
import MiniLaTeX
import Parser.Expression
import Test exposing (describe, test)


testUpdateFunction : String -> String -> String -> Test.Test
testUpdateFunction desc text1 text2 =
    let
        lines1 =
            text1 |> String.lines

        lines2 =
            text2 |> String.lines

        data1 =
            MiniLaTeX.init 0 "nada" lines1

        data2 =
            MiniLaTeX.update 1 "nada" lines2 data1

        expectedData2 =
            MiniLaTeX.init 1 "nada" lines2

        parsed2 =
            data2.parsedText |> Parser.Expression.stripList2 |> Debug.log "ACTUAL"

        expectedParsed2 =
            expectedData2.parsedText |> Parser.Expression.stripList2 |> Debug.log "EXPECTED"
    in
    test desc <|
        \_ -> Expect.equal parsed2 expectedParsed2


suite =
    describe "The ParserTool module"
        [ describe "many"
            [ testUpdateFunction "Change in one line of text"
                "This is a test"
                "This is a toast"
            , testUpdateFunction "Change in inline math"
                "Pythagoras said: $a^2 + b^2 = c^3$"
                "Pythagoras said: $a^2 + b^2 = c^2$"
            , testUpdateFunction "Error in inline math"
                "Pythagoras said: $a^2 + b^2 = c^3 foo bar baz"
                "Pythagoras said: $a^2 + b^2 = c^2$ foo bar baz"
            , testUpdateFunction "Change in display math"
                "The teacher said:$$\nx^2\n$$\nfoo bar baz"
                "The teacher said:$$\ny^2\n$$\nfoo bar baz"
            , testUpdateFunction "Error in display math"
                "The teacher said:$$\nx^2\n$\nfoo bar baz"
                "The teacher said:$$\nx^2\n$$\nfoo bar baz"
            , testUpdateFunction "Change in macro"
                "The teacher said it is \\strong{important}. Foo bar baz."
                "The teacher said it is \\strong{OK}. Foo bar baz."
            , testUpdateFunction "Error in macro"
                "The teacher said it is \\strong{important. Foo bar baz."
                "The teacher said it is \\strong{important}. Foo bar baz."
            , testUpdateFunction "empty > theorem"
                ""
                "\\begin{theorem}\nMany primes!\n\\end{theorem}\n"
            , testUpdateFunction "empty + \n > theorem"
                "\n"
                "\\begin{theorem}\nMany primes!\n\\end{theorem}\n"
            , testUpdateFunction "one line > theorem"
                "ho ho ho!\n"
                "\\begin{theorem}\nMany primes!\n\\end{theorem}\n"
            , testUpdateFunction "theorem > theorem2"
                "\\begin{theorem}\nMany primes!\n\\end{theorem}\n"
                "\n\n\\begin{theorem}\nInfinitely many primes!\n\\end{theorem}\n"
            , testUpdateFunction "error in macro > macro"
                "\\italic{foo bar baz\n"
                "\\italic{foo} bar baz\n"
            , testUpdateFunction "error in theorem > theorem"
                "\\begin{theorem}\nMany primes!\n\\end{theorem\n"
                "\n\n\\begin{theorem}\nMany primes!\n\\end{theorem}\n"
            ]
        ]
