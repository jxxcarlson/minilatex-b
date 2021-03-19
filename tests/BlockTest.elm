module BlockTest exposing (suite)

import Expect
import Parser.Block exposing (compile)
import Test exposing (describe, fuzz, test)


ex1 =
    """
Pythagoras said that

$$a^2 + b^2 = c^2$$

one
two
three
"""


ex2 =
    """
\\begin{foo}
a
b
c
\\end{foo}


one
two
three
"""


ex3 =
    """
\\begin{foo}
a

b

c
\\end{foo}
"""


exItemized =
    """
\\begin{itemize}

\\item Eggs

\\item Milk

\\item Butter

\\end{itemize}
"""


suite =
    describe "The Parser.Block module"
        [ test "compile for formula and multi-line text" <|
            \_ ->
                ex1
                    |> compile 0
                    |> Expect.equal [ [ "Pythagoras said that" ], [ "$$a^2 + b^2 = c^2$$" ], [ "one", "two", "three" ], [] ]
        , test "environment" <|
            \_ ->
                ex2
                    |> compile 0
                    |> Expect.equal [ [ "\\begin{foo}", "a", "b", "c", "\\end{foo}" ], [ "one", "two", "three" ], [] ]
        , test "environment with blank lines" <|
            \_ ->
                ex3
                    |> compile 0
                    |> Expect.equal [ [ "\\begin{foo}", "a", "", "b", "", "c", "\\end{foo}" ], [] ]
        , test "itemized environment" <|
            \_ ->
                exItemized
                    |> compile 0
                    |> Expect.equal [ [ "\\begin{itemize}", "", "\\item Eggs", "", "\\item Milk", "", "\\item Butter", "", "\\end{itemize}" ], [] ]
        ]
