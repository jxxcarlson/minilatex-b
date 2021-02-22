module Parser.Data exposing (..)


doc1 = "This is a test: $a^2 = b^2$"

doc2 = """This is a test: $a^2 = b^2$".

So is this
"""

doc3 = """AAA
BBB

$$
a^2 = b^2
$$

CCC
DDD
"""

doc4 = """
This is a test

\\begin{theorem}
Many primes!
\\end{theorem}

the end
"""
