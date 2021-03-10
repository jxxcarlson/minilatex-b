# Architecture of the MiniLaTeX Compiler


The MiniLaTeX provides two mddules, `MiniLaTeX` and 
`LaTeXMsg`.  The function

```elm
   MiniLaTeX.compile : String -> List (Html LaTeXMsg)
```

is sufficient to render static MiniLaTeX documents.
For interatictive editing and rendering applications,
on usually uses a more sophisticated API in which
only that text which was changed in the last edit
is re-parsed and re-rendered.  For this one uses

```elm
   initWithString : Int -> String -> String -> LaTeXData
```

and

```elm
   updateWithString : Int -> String -> String -> LaTeXData -> LaTeXData
```

A value of type `LaTeXData` holds all the information
needed for these computations:

```elm
type alias LaTeXData =
    { lines : List String
    , blocks : List String
    , generations : List Int
    , parsedText : List (List Expression)
    , sourceMapIndex : List (List Int)
    , renderedText : List (Html LaTeXMsg)
    , laTeXState : LaTeXState
    }
```


## Parser

The parser is best understood from the definition
of its abstract syntax tree:


## Renderer


