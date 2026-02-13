module ConversionTest exposing (suite)

import Expect
import MiniLaTeX
import Scripta.ToString
import Test exposing (describe, test)


identityTest label input =
  test label <|
         \_ ->   Expect.equal input (Scripta.ToString.fromString input |> Debug.log label)

ioTest label input output =
  test label <|
         \_ ->   Expect.equal output (Scripta.ToString.fromString input |> Debug.log label)

suite =
    describe "Scripta.FromLaTeX.convertFromString"
        [  identityTest "Plain text" "This is a test"
          , identityTest "Inline math" "$x^2$"
          , ioTest  "Display math" "$$x^2$$" """$$\nx^2\n$$"""
          , ioTest "Macro test" "\\italic{Foo}"  "[italic Foo]"
          , ioTest "Env test" """\\begin{theorem}\nThere are infinitely many primes\n\\end{theorem}"""
              """| theorem\nThere are infinitely many primes\n"""
          , ioTest "Env test (lemma)" """\\begin{lemma}\nThere are infinitely many primes\n\\end{lemma}"""
                        """| lemma\nThere are infinitely many primes\n"""
          , ioTest "complexParagraph" """This is a \\bold{test}""" """This is a [bold test]"""
          , ioTest "itemized list" itemizedListIN itemizedListOUT
        ]


itemizedListIN = """
\\begin{itemize}

\\item One

\\item Two

\\item Three

\\end{itemize}
"""

itemizedListOUT = """
- One

- Two

- Three
"""