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
  Lexer.lean         — tokenizer entry point (imports sub-modules, exposes tokenize)
  Lexer/
    Types.lean       — SourcePos, SourceSpan, TokenKind, Token, LexError
    Keywords.lean    — Python 3.12 keyword enum + lookup
    Operators.lean   — Operator and Delimiter enums
    State.lean       — LexerState, LexerM monad, char peek/advance helpers
    Char.lean        — Character classification (isIdentStart, isHexDigit, etc.)
    Number.lean      — Integer/float/imaginary literal lexing
    StringLit.lean   — String/bytes/raw/f-string literal lexing
    Indent.lean      — INDENT/DEDENT generation from leading whitespace
    Core.lean        — Main tokenization loop, operator dispatch
  AST.lean           — AST umbrella import
  AST/
    Types.lean       — Expr, Stmt, Module, and supporting types (mutual inductives)
  Parser.lean        — parser umbrella import
  Parser/
    Types.lean       — ParseError, ParserState, ParserM monad
    Combinators.lean — PEG combinators (attempt, many, sepBy, etc.)
    Tokens.lean      — Token matching helpers (expectKeyword, parseName, etc.)
    Expr.lean        — Expression parser (18 precedence levels)
    Stmt.lean        — Statement parser (simple + compound statements)
    Core.lean        — Entry point: parse : String → Except String Module
  Interpreter.lean   — tree-walking interpreter (stub)
  Runtime.lean       — runtime support (stub)
Main.lean            — CLI entry point
LythonTest.lean      — test driver root
LythonTest/
  Basic.lean         — lexer tests (keywords, operators, numbers, strings, indent)
  Parser.lean        — parser tests (expressions, statements, integration)
```

## Code Style

- `set_option autoImplicit false` at the top of every file
- No trailing whitespace
- Follow existing patterns in the codebase
- Do NOT use `/-! ... -/` module doc comments inside `mutual` blocks (they are commands, not comments; use `-- ...` instead)
- Use `partial def` for all recursive parser functions
- Structures cannot be in `mutual` blocks; use `inductive Foo where | mk : ...` instead

## Development Plan

See `PLAN.md` for the full phased development plan.
