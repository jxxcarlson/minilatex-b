module Parser.TestData exposing (..)


env1 =
    """\\begin{theorem}
Many primes!
\\end{theorem}"""


env2 =
    """\\begin{yada}
Many primes!
\\end{yada}"""


x1 =
    """\\begin{theorem}
$p^3 = 2$ 
\\end{theorem}"""


env2b =
    """
Many primes!
\\end{yada}"""


env3 =
    """\\begin{yada}
$a^2$
\\end{yada}"""


doc1 =
    "This is a test: $a^2 = b^2$"


doc2 =
    """This is a test: $a^2 = b^2$".

So is this
"""


doc3 =
    """AAA
BBB

$$
a^2 = b^2
$$

CCC
DDD
"""


doc4 =
    """
This is a test

\\begin{theorem}
Many primes!
\\end{theorem}

the end
"""
