# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

MiniLaTeX is an Elm 0.19 package that compiles a subset of LaTeX to HTML. It supports both static compilation and efficient interactive editing with differential updates (only re-parses changed blocks).

## Commands

**Run tests:**
```bash
npx --no-install elm-test
```

**Build the demo app:**
```bash
cd app && elm make src/Main.elm --output=Main.js
```

**Run benchmarks:**
```bash
npx --no-install elm-test benchmarks/
```

## Architecture

### Compilation Pipeline

```
Source text (List String)
  → Parser.Document.process  (split into logical blocks: paragraphs, math, environments)
  → Parser.Parser            (parse each block into Expression AST)
  → Render.Accumulator       (render blocks to Html while accumulating LaTeXState)
  → List (Html LaTeXMsg)
```

### Differential Update (MiniLaTeX.update)

For interactive editing, `update` avoids full reparse:
1. Recompute blocks from edited text
2. Diff old vs new blocks using `Compiler.GenericDiffer.diff`
3. Identify changed block range
4. Reparse only changed blocks
5. Splice: `unchanged_before + reparsed_delta + unchanged_after`

### Module Layers

**Public API** (3 exposed modules): `MiniLaTeX`, `LaTeXMsg`, `Render.TextMacro`

**Parser layer** (`src/Parser/`):
- `Document.elm` — document-level state machine, block segmentation
- `Block.elm` — classifies lines into BlockTypes (TextBlock, MathBlock, EnvBlock)
- `Parser.elm` — core expression parser using `elm/parser` (Parser.Advanced)
- `Expression.elm` — AST type definition with SourceMap for bidirectional source linking
- `TextCursor.elm` — parsing state: remaining text, accumulated parsed expressions, stack

**Render layer** (`src/Render/`):
- `Render.elm` — converts Expression AST to Html
- `Accumulator.elm` — folds over blocks, threading LaTeXState through rendering
- `Reduce.elm` — updates LaTeXState (counters, cross-refs, macros) from expressions
- `LaTeXState.elm` — state type: counters, crossReferences, tableOfContents, macro dictionaries

**Compiler utilities** (`src/Compiler/`):
- `GenericDiffer.elm` — generic list diff producing DiffRecord (common prefix/suffix + deltas)
- `Differ.elm` — block-level diffing

### Key Types

**Expression** (AST node) — every variant carries a `SourceMap` with blockOffset, offset, length, content, and generation number for source ↔ rendered linking.

**LaTeXData** — the central data structure for interactive editing, carrying lines, blocks, parsed AST, rendered HTML, source map index, and LaTeXState.

**LaTeXState** — accumulated state threaded through rendering: section/equation counters, cross-references, table of contents, text and math macro dictionaries.

### Grammar

See `src/Parser/Grammar.md`. The grammar is context-sensitive (terminal symbols appear on left-hand side of productions). Key productions:
- `macro → MacroName optArgs args` (e.g., `\textbf{hello}`)
- `env EnvName → begin(EnvName) args expr end(EnvName)`
- `text → Word+`

## Testing

Tests are in `tests/` with 6 modules: ParserTest, DocumentTest, BlockTest, ParserToolTest, DifferTest, MiniLaTeXTest. Test helpers in `Parser.TestHelper` provide `roundTripCheck` for parse-recompose verification.

## Demo Apps

- `app/` — full-featured editor with side-by-side source/rendered view, differential updates, source map clicking
- `simple-demo/` — minimal example using `MiniLaTeX.compileFromString`
