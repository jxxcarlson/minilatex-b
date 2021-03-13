module Data exposing (document)


document1 =
    """
\\strong{\\italic{Note.}} This app is a demo. \\italic{Nice!}
"""


document =
    """
\\title{Examples}

\\author{James Carlson}

\\maketitle


\\tableofcontents

We can make forward references, e.g., to \\eqref{integral:xn}.

\\subheading{Examples}

\\colored{elm}{add x y = x + y}

\\bs{foo}\\texarg{bar}

\\note{I think this draft is a good start, but it needs revision}{— John}

\\image{https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcSKK5oCbORAuRM5xZjVTqEIsRGiFjTStX4euA&usqp=CAU}{Nature!}{width: 480}

\\strong{\\italic{Note.}} \\italic{This app is a demo of the simplest rendering
 features of the} \\red{MiniLaTeX library.}
 \\italic{Thus, while you can see the source text
 (left panel), the source text cannot be edited.}

 \\section{Introduction}

\\strong{MiniLatex} is a subset of LaTeX that can be
rendered live in the browser using a custom just-in-time compiler.
It can also be used for static text, as in this simple demo app
using the function \\code{MiniLaTeX.compile document}.


\\section{Formulas}

One can write in-line formulas enclosed by
single dollar signs: $a^2 + b^2 = c^2$.  For displayed
formulas, double dollar signs work:

$$
\\int_{-\\infty}^\\infty e^{-x^2} dx = \\pi
$$

And one can also use the equation environment, in which
the label macro is implemented for making cross-references:

\\begin{equation}
\\label{integral:xn}
\\int_0^1 x^n dx = \\frac{1}{n+1}
\\end{equation}

Note that you can use colors, e.g., for \\blue{blue text}
and \\red{red text}.

\\section*{Theorems}

\\subsection{Foo}

\\href{https://nytimes.com}{NYT}

\\subsubsection{Bar}

This is \\strike{important} OK.

\\begin{theorem}
There are \\highlight{infinitely} many primes $p \\equiv 1 \\text{ mod } 4$.
\\end{theorem}


We can make backward references, e.g., to \\eqref{integral:xn}.


"""
