# LeanPython

Python 3.12 interpreter in Lean4, targeting the leanSpec Ethereum consensus spec.

## Build Commands

- `lake build` — build the LeanPython library and executable
- `lake test` — run tests (builds LeanPythonTest driver; `#guard` failures = build errors)
- `lake exe leanPython` — run the interpreter

## Project Structure

```
LeanPython.lean          — umbrella import for the library
LeanPython/
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
  Interpreter.lean   — interpreter umbrella import
  Interpreter/
    Types.lean       — InterpreterState, InterpM monad, scope/heap helpers
    Eval.lean        — evalExpr + execStmt (mutual block), method dispatch
    Core.lean        — Entry point: interpret : String → IO (Except String (List String))
  Runtime.lean       — runtime umbrella import
  Runtime/
    Types.lean       — Value, HeapRef, HeapObject, FuncData, RuntimeError, Scope
    Ops.lean         — Operator dispatch, truthiness, equality, comparison, iteration
    Builtins.lean    — Built-in function implementations (print, len, range, etc.)
  Stdlib.lean        — stdlib umbrella import
  Stdlib/
    Math.lean        — Pure math helpers (ceil, floor, sqrt, log, etc.)
    Struct.lean      — struct.pack/unpack/calcsize (binary data packing)
    IO.lean          — io.BytesIO/StringIO (in-memory stream objects)
    Bisect.lean      — bisect_left/bisect_right/insort (binary search)
    Base64.lean      — b64encode/decode, b16encode/decode, urlsafe variants
    Json.lean        — json.dumps (serializer) and json.loads (parser)
    Hashlib.lean     — hashlib: SHA-256, SHAKE-128 (pure Lean4), hash object dispatch
    Hmac.lean        — hmac: HMAC-SHA256, HMAC object dispatch
    Secrets.lean     — secrets: token_bytes, randbelow (using IO.rand)
    Sys.lean         — sys: exit, TextIOWrapper for stdout/stderr
    Os.lean          — os: getcwd, getenv, listdir; os.path: join, exists, dirname, etc.
    Time.lean        — time: time, monotonic, sleep
    Datetime.lean    — datetime: datetime, timedelta, timezone classes
    Pathlib.lean     — pathlib: Path class with name/parent/stem/suffix/exists/etc.
    Logging.lean     — logging: Logger, getLogger, basicConfig, level constants
Main.lean            — CLI entry point (reads .py file, parses, interprets)
LeanPythonTest.lean      — test driver root
LeanPythonTest/
  Basic.lean         — lexer tests (keywords, operators, numbers, strings, indent)
  Parser.lean        — parser tests (expressions, statements, integration)
  Interpreter.lean   — interpreter tests (#eval-based, IO assertions)
  Module.lean        — module system tests (imports, packages, relative imports)
  Stdlib.lean        — stdlib module tests (math, copy, functools, collections, etc.)
```

## Code Style

- `set_option autoImplicit false` at the top of every file
- No trailing whitespace
- Follow existing patterns in the codebase
- Do NOT use `/-! ... -/` module doc comments inside `mutual` blocks (they are commands, not comments; use `-- ...` instead)
- Use `partial def` for all recursive parser/interpreter functions
- Structures cannot be in `mutual` blocks; use `inductive Foo where | mk : ...` instead
- `Std.HashMap` and `Std.HashSet` require `import Std.Data.HashMap` / `import Std.Data.HashSet`; use `{}` for empty (not `.empty`)
- Lean 4.29: `String.drop` returns `String.Slice`; use `String.ofList (s.toList.drop n)` for `String` result
- Lean 4.29: `Array.eraseIdx` requires bounds proof; use `(arr.toList.take i ++ arr.toList.drop (i+1)).toArray` instead

## Development Plan

See `PLAN.md` for the full phased development plan.
