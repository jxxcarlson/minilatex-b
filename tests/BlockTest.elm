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


testCompile : String -> String -> List (List String) -> Test.Test
testCompile desc text expectedBlocks =
    test desc <|
        \_ ->
            Expect.equal (compile 0 (String.lines text)) expectedBlocks


suite =
    describe "Nothing"
        [ test "zero" <| \_ -> Expect.equal 2 2
        ]


suite2 =
    describe "The Parser.Block module"
        [ describe "Block.compile"
            [ --testCompile "compile for formula and multi-line text"
              --    ex1
              --    [ [ "Pythagoras said that" ]
              --    , [ "$$a^2 + b^2 = c^2$$" ]
              --    , [ "one", "two", "three" ]
              --    ]
              --, testCompile "environment"
              --    ex2
              --    [ [ "\\begin{foo}", "a", "b", "c", "\\end{foo}" ]
              --    , [ "one", "two", "three" ]
              --    ]
              --, testCompile "environment with blank lines"
              --    ex3
              --    [ [ "\\begin{foo}"
              --      , "a"
              --      , ""
              --      , "b"
              --      , ""
              --      , "c"
              --      , "\\end{foo}"
              --      ]
              --    ]
              --, testCompile "itemized environment"
              --    exItemized
              --    [ [ "\\begin{itemize}"
              --      , ""
              --      , "\\item Eggs"
              --      , ""
              --      , "\\item Milk"
              --      , ""
              --      , "\\item Butter"
              --      , ""
              --      , "\\end{itemize}"
              --      ]
              --    ]
              --, testCompile "math block and text separated by blank line"
              --    "$$\nx^2\n$$\n\none two\n"
              --    [ [ "$$", "x^2", "$$" ]
              --    , [ "one two" ]
              --    ]
              --, testCompile
              --    "unbalanced display math, no blank lines"
              --    "$$\nx^2\n$\none two\n"
              --    [ [ "$$", "x^2", "$" ]
              --    , [ "one two" ]
              --    ]
              --, testCompile "unbalanced display math (2), no blank lines"
              --    "$$\nx^2\n$\none two\n$$\ny^2\n$"
              --    [ [ "$$", "x^2", "$" ]
              --    , [ "one two" ]
              --    , [ "$$", "y^2", "$" ]
              --    ]
              --, testCompile "Paragraphs"
              --    "A\nB\nC\n\nX\nY\nZ"
              --    [ [ "A", "B", "C" ]
              --    , [ "X", "Y", "Z" ]
              --    ]
              --, testCompile "balanced display math, no blank lines"
              --    "$$\nx^2\n$$\none two\n"
              --    [ [ "$$", "x^2", "$$" ]
              --    , [ "one two" ]
              --    ]
              --, testCompile "simple display math block"
              --    "$$\nx^2\n$$"
              --    [ [ "$$", "x^2", "$$" ]
              --    ]
              --, testCompile "simple environment"
              --    "\\begin{theorem}\nMany primes!\n\\end{theorem}"
              --    [ [ "\\begin{theorem}", "Many primes!", "\\end{theorem}" ] ]
              --, testCompile "nested environment"
              --    "\\begin{indent}\n\\begin{theorem}\nMany primes!\n\\end{theorem}\n\\end{indent}"
              --    [ [ "\\begin{indent}", "\\begin{theorem}", "Many primes!", "\\end{theorem}", "\\end{indent}" ] ]
              --, testCompile "simple environment with unclosed begin-end pair"
              --    "\\begin{theorem}\nMany primes!\n\\end{theorem"
              --    [ [ "\\begin{theorem}", "Many primes!", "\\end{theorem" ] ]
              testCompile "simple environment with unclosed begin-end pair and following text"
                "\\begin{theorem}\nMany primes!\n\\end{theorem\n\nfoo, bar"
                [ [ "\\begin{theorem}", "Many primes!", "\\end{theorem", "", "foo, bar" ] ]
            ]
        ]
