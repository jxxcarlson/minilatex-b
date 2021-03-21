# Architecture of the MiniLaTeX Compiler

_This document is a work-in-progress._

The MiniLaTeX package provides two top-level modules, `MiniLaTeX` and 
`LaTeXMsg`.  The function

```elm
   MiniLaTeX.compile : String -> List (Html LaTeXMsg)
```

is sufficient to render static MiniLaTeX documents.
For interactive editing and rendering applications,
one usually uses a more sophisticated API in which
only that text which was changed in the last edit
is re-parsed and re-rendered.  For this, use

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
## Overview

Consider the function

```elm
MiniLaTeX.initWithString : Int -> String -> String -> LaTeXData
initWithString generation selectedId input = ...
```

The task of rendering its input string takes place in stages. 
First is to compute a `State`, which looks like this:

```elm
type alias State =
    { input : List String
    , lineNumber : Int
    , generation : Int
    , blockType : BlockType
    , blockContents : List String
    , blockTypeStack : List BlockType
    , output : List TextCursor
    , laTeXState : LaTeXState
    }
```


## Parser

The parser is best understood from the definition
of its abstract syntax tree:

```elm
type Expression
    = Text String SourceMap
    | InlineMath String SourceMap
    | DisplayMath String SourceMap
    | Macro String (Maybe Expression) (List Expression) SourceMap
    | Environment String (List Expression) Expression SourceMap 
    | NewCommand String Int Expression SourceMap
    | LXList (List Expression)
    | LXError String Problem SourceMap
    | LXInstruction Instr SourceMap
```

See the file `./src/Parser/Grammar.md` for a description
of the grammar underlying the syntax tree.

### Modules

```
    Block.elm
    Document.elm
    Expression.elm
    Grammar.md
    Helpers.elm
    Parser.elm
    Problem.elm
    TestData.elm
    TestHelper.elm
    TextCursor.elm
```
## Renderer


### Modules

```
    Accumulator.elm
    LaTeXState.elm
    Reduce.elm
    ReducerHelper.elm
    Render.elm
```


