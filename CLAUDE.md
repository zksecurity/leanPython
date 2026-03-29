# Lython

Python 3.12 interpreter in Lean4, targeting the leanSpec Ethereum consensus spec.

## Build Commands

- `lake build` — build the Lython library and executable
- `lake test` — run tests (builds LythonTest driver; `#guard` failures = build errors)
- `lake exe lython` — run the interpreter

## Project Structure

```
Lython.lean          — umbrella import for the library
Lython/
  Lexer.lean         — tokenizer
  Parser.lean        — PEG parser
  AST.lean           — Python AST node types
  Interpreter.lean   — tree-walking interpreter
  Runtime.lean       — runtime support (types, exceptions, stdlib)
Main.lean            — CLI entry point
LythonTest.lean      — test driver root
LythonTest/
  Basic.lean         — smoke tests
```

## Code Style

- `set_option autoImplicit false` at the top of every file
- No trailing whitespace
- Follow existing patterns in the codebase

## Development Plan

See `PLAN.md` for the full phased development plan.
