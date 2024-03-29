module Data exposing (document, minilatexio)


document1 =
    """
\\strong{\\italic{Note.}} This app is a demo. \\italic{Nice!}
"""


minilatexio =
    """


\\title{Sample MiniLaTeX Doc}

\\begin{mathmacro}
\\newcommand{\\bt}[1]{\\bf{#1}}
\\newcommand{\\mca}[0]{\\mathcal{A}}
\\end{mathmacro}

\\begin{textmacro}
\\newcommand{\\boss}{Phineas Fogg}
\\newcommand{\\hello}[1]{Hello \\strong{#1}!}
\\newcommand{\\reverseconcat}[3]{#3#2#1}
\\end{textmacro}



\\maketitle

% EXAMPLE 1

\\tableofcontents

\\section{Introduction}


Macro of zero arguments: \\foo .


MiniLatex is a subset of LaTeX that can be
rendered live in the browser using a custom just-in-time compiler.
Mathematical text is rendered by \\href{https://mathjax.org}{MathJax}:

\\begin{theorem}
Euler sez:
$$
\\int_{-\\infty}^\\infty e^{-x^2} dx = \\pi
$$
\\end{theorem}

\\begin{center}
The following environment is nested:
\\begin{bar}
Ho ho ho!
\\end{bar}
\\end{center}


The combination of MiniLaTeX and MathJax
gives you access to both text-mode
and math-mode LaTeX in the browser.

\\begin{colored}[elm]
foo : Int -> Int -> Int
add x y = x + y
\\end{colored}

\\begin{comment}
This is a long comment.
It should not appear in the
rendered text.
\\end{comment}git

\\begin{defitem}[Bug]
Update on other than "point" changes not working.
\\end{defitem}


\\begin{defitem}[Bug]
Highlight macro not working.
\\end{defitem}


\\begin{defitem}[Bug]
Text mode macros inside other macros are not expended.
\\end{defitem}

\\begin{defitem}[Bug]
Display math not centered
\\end{defitem}

\\begin{defitem}[Bug]
Syntax colors not displayed.  CSS not read?
\\end{defitem}

\\begin{obeylines}
one
two
three
\\end{obeylines}

\\begin{indent}
The combination of MiniLaTeX and MathJax
gives you access to both text-mode
and math-mode LaTeX in the browser.
\\end{indent}



\\begin{center}
MiniLatex is a subset of LaTeX that can be
rendered live in the browser using a custom just-in-time compiler.
Mathematical text is rendered by \\href{https://mathjax.org}{MathJax}:
\\end{center}


Feel free to
experiment with MiniLatex using this app
\\mdash you can change the text in the
left-hand window, or clear it and enter
your own text. Use the \\blue{export} button
below to export the text you write to a
LaTeX document on your computer.  It can
be processed as-is by any program that runs LaTeX,
e.g, TeXShop or \\code{pdflatex}.


Images in MiniLaTeX are accessed by URL (see the example
in section 4 below). When you export a document, images
used in it will be listed to the right
of the rendered text.  To use them in the exported
document, right (option) click on the image and
save it in a folder named \\italic{image}.

For more information about
the MiniLaTeX project, please go to
\\href{https://minilatex.io}{minilatex.io},
or write to jxxcarlson at gmail.

\\section{Try it out}

\\italic{Try editing formula \\eqref{integral:xn} or \\eqref{integral:exp} below.}
Note, e.g., from the source text, that the formulas are written inside
equation environments.

The most basic integral:

\\begin{equation}
\\label{integral:xn}
\\int_0^1 x^n dx = \\frac{1}{n+1}
\\end{equation}

An improper integral:

\\begin{equation}
\\label{integral:exp}
\\int_0^\\infty e^{-x} dx = 1
\\end{equation}

\\section{Theorems}

\\begin{theorem}
There are infinitely many prime numbers.
\\end{theorem}

\\begin{theorem}
There are infinitley many prime numbers
$p$ such that $p \\equiv 1\\ mod\\ 4$.
\\end{theorem}

\\section{Images}

\\image{http://psurl.s3.amazonaws.com/images/jc/beats-eca1.png}{Figure 1. Two-frequency beats}{width: 350, align: center}

\\section{SVG}

\\begin{svg}
<svg version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0" y="0" width="381.603" height="250.385" viewBox="0, 0, 381.603, 250.385">
  <g id="Layer_1">
    <g>
      <path d="M95.401,166.09 L71.5,124.692 L95.401,83.295 L143.203,83.295 L167.103,124.692 L143.202,166.09 z" fill="#CCDD10"/>
      <path d="M95.401,166.09 L71.5,124.692 L95.401,83.295 L143.203,83.295 L167.103,124.692 L143.202,166.09 z" fill-opacity="0" stroke="#000000" stroke-width="1"/>
    </g>
    <g>
      <path d="M166.401,126.09 L142.5,84.692 L166.401,43.295 L214.203,43.295 L238.103,84.692 L214.202,126.09 z" fill="#0D9B53"/>
      <path d="M166.401,126.09 L142.5,84.692 L166.401,43.295 L214.203,43.295 L238.103,84.692 L214.202,126.09 z" fill-opacity="0" stroke="#000000" stroke-width="1"/>
    </g>
    <g>
      <path d="M167.401,207.885 L143.5,166.487 L167.401,125.09 L215.203,125.09 L239.103,166.487 L215.202,207.885 z" fill="#0D9B53"/>
      <path d="M167.401,207.885 L143.5,166.487 L167.401,125.09 L215.203,125.09 L239.103,166.487 L215.202,207.885 z" fill-opacity="0" stroke="#000000" stroke-width="1"/>
    </g>
    <g>
      <path d="M309.401,209.885 L285.5,168.487 L309.401,127.09 L357.203,127.09 L381.103,168.487 L357.203,209.885 z" fill="#0D9B53"/>
      <path d="M309.401,209.885 L285.5,168.487 L309.401,127.09 L357.203,127.09 L381.103,168.487 L357.203,209.885 z" fill-opacity="0" stroke="#000000" stroke-width="1"/>
    </g>
    <g>
      <path d="M309.401,125.09 L285.5,83.692 L309.401,42.295 L357.203,42.295 L381.103,83.692 L357.203,125.09 z" fill="#0D9B53"/>
      <path d="M309.401,125.09 L285.5,83.692 L309.401,42.295 L357.203,42.295 L381.103,83.692 L357.203,125.09 z" fill-opacity="0" stroke="#000000" stroke-width="1"/>
    </g>
    <g>
      <path d="M23.401,126.09 L-0.5,84.692 L23.401,43.295 L71.203,43.295 L95.103,84.692 L71.203,126.09 z" fill="#0D9B53"/>
      <path d="M23.401,126.09 L-0.5,84.692 L23.401,43.295 L71.203,43.295 L95.103,84.692 L71.203,126.09 z" fill-opacity="0" stroke="#000000" stroke-width="1"/>
    </g>
    <g>
      <path d="M237.401,84.295 L213.5,42.897 L237.401,1.5 L285.203,1.5 L309.103,42.897 L285.203,84.295 z" fill="#CCDD10"/>
      <path d="M237.401,84.295 L213.5,42.897 L237.401,1.5 L285.203,1.5 L309.103,42.897 L285.203,84.295 z" fill-opacity="0" stroke="#000000" stroke-width="1"/>
    </g>
    <g>
      <path d="M239.401,249.885 L215.5,208.487 L239.401,167.09 L287.203,167.09 L311.103,208.487 L287.203,249.885 z" fill="#CCDD10"/>
      <path d="M239.401,249.885 L215.5,208.487 L239.401,167.09 L287.203,167.09 L311.103,208.487 L287.203,249.885 z" fill-opacity="0" stroke="#000000" stroke-width="1"/>
    </g>
    <g>
      <path d="M94.401,84.295 L70.5,42.897 L94.401,1.5 L142.203,1.5 L166.103,42.897 L142.202,84.295 z" fill="#CCDD10"/>
      <path d="M94.401,84.295 L70.5,42.897 L94.401,1.5 L142.203,1.5 L166.103,42.897 L142.202,84.295 z" fill-opacity="0" stroke="#000000" stroke-width="1"/>
    </g>
    <g>
      <path d="M333.302,94.897 C327.411,94.897 322.635,90.328 322.635,84.692 C322.635,79.056 327.411,74.487 333.302,74.487 C339.193,74.487 343.968,79.056 343.968,84.692 C343.968,90.328 339.193,94.897 333.302,94.897 z" fill="#D60B8E"/>
      <path d="M333.302,94.897 C327.411,94.897 322.635,90.328 322.635,84.692 C322.635,79.056 327.411,74.487 333.302,74.487 C339.193,74.487 343.968,79.056 343.968,84.692 C343.968,90.328 339.193,94.897 333.302,94.897 z" fill-opacity="0" stroke="#000000" stroke-width="1"/>
    </g>
  </g>
</svg>
\\end{svg}


\\section{Lists}

\\begin{itemize}

\\item This is \\strong{just} a test.

\\item \\italic{And so is this:} $a^2 + b^2 = c^2$

\\begin{itemize}

\\item Items can be nested

\\item And you can do this: $ \\frac{1}{1 + \\frac{2}{3}} $

\\end{itemize}

\\end{itemize}



\\section{Tables}

\\begin{indent}
\\begin{tabular}{ l l l l }
Hydrogen & H  \\\\
Helium & He \\\\
Lithium& Li  \\\\
Beryllium& Be
\\end{tabular}
\\end{indent}


\\section{Math-mode macros}

Math-mode macros are added using the \\code{mathmacro} environment:

\\begin{verbatim}
\\begin{mathmacro}
\\newcommand{\\bt}[1]{\\bf{#1}}
\\newcommand{\\mca}[0]{\\mathcal{A}}
\\end{mathmacro}
\\end{verbatim}

Then saying

\\begin{verbatim}
 $$
 a^2 = \\bt{Z}/\\mca
 $$
\\end{verbatim}

yields

$$
a^2 = \\bt{Z}/\\mca
$$

Likewise, saying

\\begin{verbatim}
\\begin{equation}
\\label{eq:function.type}
\\mca^{\\bt{Z}} = \\bt{Z} \\to \\mca
\\end{equation}
\\end{verbatim}

yields

\\begin{equation}
\\label{eq:function.type}
\\mca^{\\bt{Z}} = \\bt{Z} \\to \\mca
\\end{equation}

There are some restrictions:

\\begin{verbatim}
1. No regions, e.g, use \\bf{#1},
   not {\\bf #1}

2. Macros with no arguments:
   Say \\newcommand{\\mca}[0]{\\mathcal{A}},
   not \\newcommand{\\mca}{\\mathcal{A}}

3. No recursion: the body of the macro
   cannot refer to other macros defined
   in the mathmacro environment.

4. Put the mathmacro environment at
   the beginning of the document
\\end{verbatim}

Items 1—3 will be eliminated in a
future release.

\\section{Text-mode Macros}

Text-mode macros are defined in a \\code{textmacro} environment:

\\begin{verbatim}
\\begin{textmacro}
\\newcommand{\\boss}{Phineas Fogg}
\\newcommand{\\hello}[1]{Hello \\strong{#1}!}
\\newcommand{\\reverseconcat}[3]{#3#2#1}
\\end{textmacro}
\\end{verbatim}

Then

\\begin{verbatim}
My boss is \\boss.
\\end{verbatim}

My boss is \\boss.


Likewise, the text

\\begin{verbatim}
\\hello{John}
\\end{verbatim}

yields \\hello{John}.

\\strong{Example:} \\reverseconcat{A}{B}{C}

\\section{MiniLatex Macros}

MiniLatex has a number of macros of its own,  For
example, text can be rendered in various colors, \\red{such as red}
and \\blue{blue}. Text can \\highlight{be highlighted}
and can \\strike{also be struck}. Here are the macros:

\\begin{verbatim}
\\red
\\blue
\\highlight
\\strike
\\end{verbatim}

\\section{Errors and related matters}

Errors, as illustrated below, are rendered in real time and are reported in red, in place.
For example, suppose you type the  text

\\begin{verbatim}
  $$
  a^2 + b^2 = c^2
\\end{verbatim}

Then you will see this in the rendered text window:

\\image{http://jxxcarlson.s3.amazonaws.com/miniLaTeXErrorMsg-2020-02-22.png}{Fig 2. Error message}{width: 200}

We plan to make further improvements in error reporting.

\\section{More about MiniLaTeX}

This app is intended as a bare-bones demonstration of what one can do with MiniLaTeX.
There are several other apps in various stages of development which
offer different or more sophisticated services:


For more information about these or related apps, please contact jxxcarlson at gmail.
Bug reports and feature requests are best posted on
the \\href{https://github.com/jxxcarlson/meenylatex}{Github repo} for this project,
but email is also OK.

\\section{The Technology}

MiniLatex is written in \\href{https://elm-lang.org}{Elm}, the statically typed functional
programming language created by Evan Czaplicki for building web applications.  Because of its excellent
\\href{https://package.elm-lang.org/packages/elm/parser/latest}{parser combinator library},
Elm is a good fit for a project like the present one.  Math-mode LaTeX is rendered
by \\href{https://mathjax.org}{MathJax}.  It is a pleasure to thank Davide Cervone for his
generous help with MathJax.

For an overview of the design of MiniLatex, see
\\href{https://hackernoon.com/towards-latex-in-the-browser-2ff4d94a0c08}{Towards Latex in the Browser}.
Briefly, \\href{https://www.mathjax.org/}{MathJax} is used for math-mode
text and Elm is used for text-mode material.

One feature of note is the default incremental
parsing and rendering of source text, which is needed for responsive live editing.
Source text is divided into logical paragraphs: ordinary paragraphs or an outer
begin-end block.  When a logical paragraph is modified, only that paragraph
is recompiled.  The upside of this strategy is that recompilation is very fast.
The downside is that numbering and cross-references can get out of sync.  Press
the \\blue{Full Render} button to recompile the entire document and bring everything
into sync.

\\href{https://github.com/jxxcarlson/meenylatex}{Github repo}

\\medskip

\\section{References}

\\begin{thebibliography}

\\bibitem{G} James Carlson, \\href{https://knode.io/628}{MiniLaTeX Grammar},

\\bibitem{H} James Carlson, \\href{https://hackernoon.com/towards-latex-in-the-browser-2ff4d94a0c08}{Towards LaTeX in the Browser }, Hackernoon

\\bibitem{S} \\href{http://package.elm-lang.org/packages/jxxcarlson/minilatex/latest}{Source code}

\\bibitem{T} James Carlson, \\href{https://knode.io/525}{MiniLatex Technical Report}

\\end{thebibliography}
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
