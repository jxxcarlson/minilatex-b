# MiniLaTeX Grammar


## The AST

```elm
type Expression
    = Text String SourceMap
    | InlineMath String SourceMap
    | DisplayMath String SourceMap
    | Macro String (Maybe String) (List Expression) SourceMap
    | Environment String (List Expression) Expression SourceMap -- Environment name optArgs body
    | LXList (List Expression)
    | LXError String Problem SourceMap
    | LXNull () SourceMap

```

## Productions

Terminals are capitalized, non-terminals are not

```elm
expr -> text
       | InlineMath
       | DisplayMath
       | macro
       | env
       | lxList
       | LxError
       | LxNull
  
text        -> Word+     
macro       -> MacroName optArgs args
optarg      -> Maybe expr
args        -> expr*
env EnvName -> begin(EnvName) args expr end(EnvName)
```

Because terminal symbols appear on the left-hand
side of productions, the grammar is contxt-sensitive.
