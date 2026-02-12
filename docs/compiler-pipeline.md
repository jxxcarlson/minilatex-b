# MiniLaTeX Compiler Pipeline

The MiniLaTeX compiler has four stages.

## 1. Block Segmentation

**Module:** `Parser.Document.process`

Raw text (a list of strings/lines) is split into logical *blocks* — contiguous paragraphs, `$$ ... $$` display math regions, and `\begin{env}...\end{env}` pairs. Each line is classified by `LineType` (blank, text, math, begin/end env), and a state machine accumulates lines into `Block` values tagged with a `BlockType`.

## 2. Expression Parsing

**Module:** `Parser.Parser`

Each block is independently parsed into a list of `Expression` AST nodes. The parser (built on `elm/parser` Advanced) recognizes: `Text`, `InlineMath` ($...$), `DisplayMath`, `Macro` (\cmd[opt]{arg}), `Environment`, `NewCommand`, and `LXError` for recovery. Every node carries a `SourceMap` (block offset, character offset, length, original content, generation number) for bidirectional source linking.

## 3. State Accumulation and Rendering

**Modules:** `Render.Accumulator`, `Render.Render`, `Render.Reduce`

Blocks are folded left-to-right, threading a `LaTeXState` through each one. `Render.Reduce` updates the state (section/equation counters, cross-references, table of contents, macro dictionaries) from each expression. `Render.Render` converts each `Expression` to `Html` using the current state — so a `\ref{foo}` resolves against cross-references accumulated so far.

## 4. Differential Update

**Module:** `MiniLaTeX.update`

For interactive editing, the full pipeline only runs once via `init`. On subsequent edits, `update` diffs old vs new blocks using `Compiler.GenericDiffer.diff` to find the longest common prefix and suffix. Only the changed blocks in the middle are re-parsed and re-rendered, then spliced back: `unchanged_before ++ reparsed_delta ++ unchanged_after`.
