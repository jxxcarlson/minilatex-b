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
          , ioTest "Inline math" "$x^2$"  "$x^2$"
          , ioTest "Inline math (2)" "\\(x^2\\)"  "$x^2$"
          , ioTest "Inline math (3)" "\\(T = b^{-1}\\)" "$T = b^{-1}$"
          , ioTest "Inline math (4)" "\\(T = \\beta^{-1}\\)" "$T = \\beta^{-1}$"
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
          , ioTest "imagemacro" imageIN imageOUT
          , ioTest "text in math" "$x \\text{if} y$" """$x " if " y$"""
          , ioTest "text in display math" "$$a \\text{where} b$$" "| math\na where b"
          , ioTest "numbered item" numberedIN numberedOUT
          , ioTest "image float right" imageFloatRightIN imageFloatRightOUT
          , ioTest "image float right2" imageFloatRightIN2 imageFloatRightOUT2
          , ioTest "textInEquation" textInEquationIN textInEquationOUT
          , ioTest "codeBlock" codeIN codeOUT
          , ioTest "bibItem" bibitemIN bibitemOUT
          , ioTest "shiftAndSetCounter" shiftAndSetCounterIN shiftAndSetCounterOUT
          , ioTest "tags" tagsIN tagsOUT
          , ioTest "content" contentsIN contentsOUT
          , ioTest "ilink" ilinkIN ilinkOUT
          , ioTest "imageStacked" imageStackedIn imageStackedOut
          , ioTest "equation2Test" equation2IN equation2OUT
          , ioTest "bibitemTest" bibItemIN bibItemOUT
        ]

bibItemIN = """\\bibitem{PTE}
\\href{http://www2.ph.ed.ac.uk/~ldeldebb/docs/QM/lect17.pdf}{Perturbation Theory, Edinburgh}"""

bibItemOUT = """| bibitem PTE
[link Perturbation Theory, Edinburgh http://www2.ph.ed.ac.uk/~ldeldebb/docs/QM/lect17.pdf]
"""

equation2IN = """\\begin{aligned}
 \\label{aa4}
 \\Delta E_0 &= \\lambda g \\left(\\frac{ \\hbar }{  2m \\omega} \\right)^{2} \\langle \\psi_0 | ( a + a^\\dagger )^4 | \\psi_0 \\rangle \\\\
 &= \\lambda \\frac{1}{16}\\frac{\\omega \\hbar}{2}\\langle \\psi_0 | ( a + a^\\dagger )^4 | \\psi_0 \\rangle
 \\end{aligned}"""

equation2OUT = """| aligned label:aa4
 \\Delta E_0 &= \\lambda g \\left(\\frac{ \\hbar }{  2m \\omega} \\right)^{2} \\langle \\psi_0 | ( a + a^\\dagger )^4 | \\psi_0 \\rangle \\\\
 &= \\lambda \\frac{1}{16}\\frac{\\omega \\hbar}{2}\\langle \\psi_0 | ( a + a^\\dagger )^4 | \\psi_0 \\rangle"""

imageStackedIn = "\\image{https://psurl.s3.amazonaws.com/images/jc/sinc2-bcbf.png  caption:Wave packet width:300}"


imageStackedOut = """| image
| caption:Wave
packet
| width:300
https://psurl.s3.amazonaws.com/images/jc/sinc2-bcbf.png"""

ilinkIN = "\\ilink{Quantum Mechanics Notes id-708d4474-8450-4061-bd96-bc55c8b71f4f}"

ilinkOUT = "[ilink Quantum Mechanics Notes id-708d4474-8450-4061-bd96-bc55c8b71f4f]"

contentsIN = "\\contents"
contentsOUT = ""

tagsIN = "\\tags{system:startup jxxcarslon:wave-packets-dispersion}"
tagsOUT = ""

shiftAndSetCounterIN = "\\shiftandsetcounter{2}"

shiftAndSetCounterOUT = ""

bibitemIN = """\\bibitem{QM}
\\link{Quantum Mechanics for Engineers: Wave Packets https://www.eng.fsu.edu/~dommelen/quantum/style_a/packets.html}
"""

bibitemOUT = """| bibitem QM
[link Quantum Mechanics for Engineers: Wave Packets https://www.eng.fsu.edu/~dommelen/quantum/style_a/packets.html]
"""

codeIN = """\\begin{code}
# jupyter/python

matplotlib inline
\\end{code}"""

codeOUT = """| code
# jupyter/python

matplotlib inline
"""

textInEquationIN = """| equation
h = 6.6\\times 10^{-34}\\;\\text{Joule-sec}"""

textInEquationOUT = """| equation
h = 6.6\\times 10^{-34}\\;\"Joule-sec\""""

imageFloatRightIN = "[image http://psurl.s3.amazonaws.com/images/jc/snell2-5b65.jpg float:right width:70]"

imageFloatRightOUT = """| image float:right width:70
http://psurl.s3.amazonaws.com/images/jc/snell2-5b65.jpg"""

imageFloatRightIN2 = "[image http://psurl.s3.amazonaws.com/images/jc/fermat_principle-94da.jpg float:right width:300]"

imageFloatRightOUT2 = """| image float:right width:300
http://psurl.s3.amazonaws.com/images/jc/fermat_principle-94da.jpg"""


numberedIN = """\\numbered
Why are atoms stable? According to \\italic{classical electromagnetic theory} ($x^2$), electrons whirling around the nucleus should radiate their energy way in the blink of an eye.
"""

numberedOUT = """. Why are atoms stable? According to [italic classical electromagnetic theory] ($x^2$), electrons whirling around the nucleus should radiate their energy way in the blink of an eye.
"""
imageIN = """\\image{http://psurl.s3.amazonaws.com/images/jc/huygens_snell-5ae9.jpg float:right width:300}"""

imageOUT = """| image\n| float:right\n| width:300\nhttp://psurl.s3.amazonaws.com/images/jc/huygens_snell-5ae9.jpg"""


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