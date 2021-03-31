module Data.Medium exposing (..)

sourceText = """
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

MiniLatex is a subset of LaTeX that can be
rendered live in the browser using a custom just-in-time compiler.
Mathematical text is rendered by \\href{https://mathjax.org}{MathJax}:

$$
\\int_{-\\infty}^\\infty e^{-x^2} dx = \\pi
$$

The combination of MiniLaTeX and MathJax
gives you access to both text-mode
and math-mode LaTeX in the browser.

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
Hydrogen & H & 1 & 1.008 \\\\
Helium & He & 2 & 4.003 \\\\
Lithium& Li & 3 & 6.94 \\\\
Beryllium& Be& 4& 9.012 \\\\
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
\\italic{My boss is \\boss.}
\\end{verbatim}

produces \\italic{My boss is \\boss.}
Likewise, the text

\\begin{verbatim}
\\hello{John}
\\end{verbatim}

yields \\hello{John}.

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

\\begin{itemize}

\\item \\href{https://knode.io}{knode.io} is a web app for creating, editing, and distributing
MiniLaTeX documents.  Try this link: \\href{https://knode.io/424}{QM class notes on knode.io}.
Public documents can be read by anyone, but to create documents, you need to create an account
on knode.io.

\\item \\href{https://reader.minilatex.app}{reader.minilatex.app} is a read-only app for
distributing MiniLaTeX documents on the web.  Good for class notes and the like.

\\item \\italic{In development}: a desktop app for creating MiniLaTeX content. Documents
are stored on your computer's hard disk and can also be stored in the cloud.
Documents can also be posted to any website that implements the app's publishing
protocol.  The desktop app supports two document formats: MiniLaTeX and MathMarkdown,
a version of Markdown that can render math-mode LaTeX.

\\end{itemize}

For more information about these or related apps, please contact jxxcarlson at gmail.
Bug reports and feature requests are best posted on
the \\href{https://github.com/jxxcarlson/meenylatex}{Github repo} for this project,
but email is also OK.

"""