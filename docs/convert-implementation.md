# Scripta.FromLaTeX.convert — Implementation Summary

## Public API

- `convert : List (List Expression) -> List ExpressionBlock` — transforms the MiniLaTeX AST into Scripta's block-based representation

## Block-level mapping (`convertBlock`)

- `DisplayMath` → `Verbatim "math"` with `body = Left str`
- Pass-through environments (equation, align, verbatim, listing, etc.) → `Verbatim` with mapped name and `body = Left str`
- Other environments → `Ordinary name` with `body = Right (convertBody ...)`
- Section macros → `Ordinary "section"` with level in args (`"1"`, `"2"`, `"3"`)
- Title/author/date/maketitle → `Ordinary` blocks
- Mixed expressions → `Paragraph` with `body = Right exprs`
- Empty blocks and all-comment blocks → dropped (`Nothing`)

## Expression-level mapping (`convertExpr`)

- `Text` → `Text`, `InlineMath` → `VFun "$"`, `DisplayMath` → `VFun "$$"`
- `Macro` → `Fun` (optional args prepended), `Item` → `Fun "item"`, `Environment` → `Fun name`
- `Comment`, `NewCommand`, `LXInstruction` → skipped

## Metadata

- `SourceMap` → `ExprMeta` (begin/end/index/id)
- Block-level `BlockMeta` built from `getSourceOfList`
