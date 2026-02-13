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

macro : String -> String -> String
macro name body = "\\" ++ name ++ "{" ++ body ++ "}"

env : String -> String -> String
env name body =
    (macro "begin" name)++ "\n" ++ body ++ "\n" ++ (macro "end" name)

block : String -> String -> String
block name body =
   "| " ++ name ++ "\n" ++ body

suite =
    describe "Scripta.FromLaTeX.convertFromString"
        [  identityTest "Plain text" "This is a test"
          , ioTest "Bock order" twoBlocksIN twoBlocksOUT
          , ioTest "Inline math" "$x^2$"  "[math x^2]"
          , ioTest "Inline math (2)" "\\(x^2\\)"  "[math x^2]"
          , ioTest "Inline math (3)" "\\(T = b^{-1}\\)" "[math T = b^{-1}]"
          , ioTest "Inline math (4)" "\\(T = \\beta^{-1}\\)" "[math T = \\beta^{-1}]"
          , ioTest "Equation environment" (env "equation" "x^2") (block "equation" "x^2")
          , ioTest "Labeled equation environment" labeledEquationIN labeledEquationOUT
          , ioTest "Display math" "$$x^2$$" """| math\nx^2"""
          , ioTest "totaEntropy" totalEntropyRelationIN totalEntropyRelationOUT
          , ioTest "Mathmcros test" mathMacrosIN mathMacrosOUT
          , ioTest "Macro test" "\\italic{Foo}"  "[italic Foo]"
          , ioTest "Env test" """\\begin{theorem}\nThere are infinitely many primes\n\\end{theorem}"""
              """| theorem\nThere are infinitely many primes"""
          , ioTest "Env test (lemma)" """\\begin{lemma}\nThere are infinitely many primes\n\\end{lemma}"""
                        """| lemma\nThere are infinitely many primes"""
          , ioTest "complexParagraph" """This is a \\bold{test}""" """This is a [bold test]"""
          , ioTest "itemized list" itemizedListIN itemizedListOUT
        ]


totalEntropyRelationIN = """
$$
\\frac{\\partial S_{AB}}{\\partial q_A} = \\frac{\\partial S_A}{\\partial q_A} + \\frac{\\partial S_B}{\\partial q_A} < 0
$$"""

totalEntropyRelationOUT = """| math
\\frac{\\partial S_{AB}}{\\partial q_A} = \\frac{\\partial S_A}{\\partial q_A} + \\frac{\\partial S_B}{\\partial q_A} < 0"""

twoBlocksIN = """\\section{One}

Blah Blah

\\section{Two}"""

twoBlocksOUT = """| section
One

Blah Blah

| section
Two"""

mathMacrosIN = """
\\begin{mathmacro}
\\newcommand{\\bt}[1]{\\mathbold{#1}}
\\newcommand{\\mca}[0]{\\mathcal{A}}
\\end{mathmacro}
"""

mathMacrosOUT= """| mathmacros
bt: \\mathbold{#1}
mca: \\mathcal{A}"""

labeledEquationIN = """
\\begin{equation}
\\label{integral-exp}
\\int_0^\\infty e^{-x} dx = 1
\\end{equation}
"""

labeledEquationOUT = "| equation label:integral-exp\n\\int_0^\\infty e^{-x} dx = 1"

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