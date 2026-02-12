# Plan: Implement `Scripta.FromLaTeX.convert`

## Context

We need to convert the MiniLaTeX AST (`List (List Parser.Expression.Expression)`) to Scripta's block-based representation (`List Scripta.Types.ExpressionBlock`). This bridges two compiler IRs: the MiniLaTeX parser output (expression-level AST with source maps) and Scripta's block+expression model (blocks with headings, metadata, and either raw strings or parsed expression lists).

## Critical Files

- **Implement**: `src/Scripta/FromLaTeX.elm`
- **Source types**: `src/Parser/Expression.elm` — `Expression`, `SourceMap`, `getSource`, `getSourceOfList`, `toString`
- **Target types**: `src/Scripta/Types.elm` — `ExpressionBlock`, `Expression`, `Heading`, `ExprMeta`, `BlockMeta`
- **Pass-through env list**: `src/Parser/Core.elm` lines 770-771

## Design Decisions (confirmed with user)

1. Section macros (`\section{Title}`) → `Ordinary "section"` block with `args = ["1"]` for level
2. DisplayMath (`$$...$$`) → `Verbatim "math"` block with `body = Left mathStr`
3. Comments and NewCommands → dropped from output entirely

## Block-Level Mapping

Each inner `List Expression` becomes one `ExpressionBlock`. Pattern-match on the list:

| Pattern | heading | body | args |
|---|---|---|---|
| `[DisplayMath str sm]` | `Verbatim "math"` | `Left str` | `[]` |
| `[Environment name _ body _]` where passthrough | `Verbatim (mapEnvName name)` | `Left (bodyStr body)` | from optArgs |
| `[Environment name opts body _]` otherwise | `Ordinary name` | `Right (convertBody body)` | from optArgs |
| `[Macro "section" opt args _]` | `Ordinary "section"` | `Right (convertArgs args)` | `["1"]` |
| `[Macro "subsection" opt args _]` | `Ordinary "section"` | `Right (convertArgs args)` | `["2"]` |
| `[Macro "subsubsection" opt args _]` | `Ordinary "section"` | `Right (convertArgs args)` | `["3"]` |
| `[Macro "title" _ args _]` | `Ordinary "title"` | `Right (convertArgs args)` | `[]` |
| `[Macro "author" _ args _]` | `Ordinary "author"` | `Right (convertArgs args)` | `[]` |
| `[Macro "maketitle" _ _ _]` | `Ordinary "maketitle"` | `Right []` | `[]` |
| Mixed expressions (default) | `Paragraph` | `Right (convertExprs list)` | `[]` |
| `[]` or all comments | Skip (return Nothing) |

**Pass-through environments** (from Parser.Core): equation, eqnarray, verbatim, colored, CD, mathmacro, textmacro, listing, verse, align, matrix, pmatrix, bmatrix, Bmatrix, vmatrix, Vmatrix

**Environment name mapping** for Verbatim headings:
- equation → "equation", align/eqnarray → "aligned", verbatim → "verbatim"
- listing/colored → "code", verse → "verse"
- mathmacro → "mathmacros", textmacro → "textmacros"
- matrix variants, CD → "math"

## Expression-Level Mapping

Each `Parser.Expression.Expression` → `Scripta.Types.Expression` (`Expr ExprMeta`):

| Source | Target |
|---|---|
| `Text str sm` | `Text str meta` |
| `InlineMath str sm` | `VFun "$" str meta` |
| `DisplayMath str sm` | `VFun "$$" str meta` (fallback if encountered inline) |
| `Macro name Nothing args sm` | `Fun name (map convertExpr args) meta` |
| `Macro name (Just opt) args sm` | `Fun name (convertExpr opt :: map convertExpr args) meta` |
| `Item k body sm` | `Fun "item" [convertExpr body] meta` |
| `Environment name opts body sm` | `Fun name (convertExprs (unwrap body)) meta` (nested env) |
| `LXList exprs` | Unwrap: convert each child, collect into list |
| `LXError str prob sm` | `Text ("Error: " ++ problemAsString prob) meta` |
| `Comment _ _` | Skip |
| `NewCommand _ _ _ _` | Skip |
| `LXInstruction _ _` | Skip |

## SourceMap → ExprMeta

```elm
toExprMeta : Int -> SourceMap -> ExprMeta
toExprMeta index sm =
    { begin = sm.offset
    , end = sm.offset + sm.length
    , index = index
    , id = String.fromInt sm.generation ++ ":" ++ String.fromInt sm.blockOffset ++ "-" ++ String.fromInt sm.offset
    }
```

## BlockMeta Construction

Built from `Parser.Expression.getSourceOfList` on the block's expressions:
- `id` = `"gen:blockOffset"`
- `position` = blockIndex (position in document)
- `lineNumber` = sm.blockOffset
- `bodyLineNumber` = sm.blockOffset + 1 (approximate)
- `numberOfLines` = count newlines in source content
- `sourceText` = reconstructed from expressions via `toString`
- `messages` = `[]`, `error` = `Nothing`

## Implementation Steps

1. **Helper functions**: `toExprMeta`, `toBlockMeta`, `dummyExprMeta`, `isPassThroughEnv`, `isSectionMacro`, `sectionLevel`, `passThroughToVerbatimName`, `unwrapBody`, `bodyToString`
2. **`convertExpr`**: Expression-level mapping with index parameter
3. **`convertExprList`**: Maps a list, threading index counter, filtering skippable expressions
4. **`convertBlock`**: Pattern-matches on inner list, builds ExpressionBlock
5. **`convert`**: `List.indexedMap convertBlock >> List.filterMap identity`

## Verification

1. `elm make src/Scripta/FromLaTeX.elm` — compiles without errors
2. `npx --no-install elm-test` — all 66 existing tests pass
3. Manual verification: in elm repl or test, run `convert (MiniLaTeX.parse 0 testDoc)` on sample documents and inspect output structure
