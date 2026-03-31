# LeanPython

**Warning: This codebase was vibe-coded with Claude. Do not use this for anything of value.**

No theorem proving is involved yet. This is plain Lean4 code that happens to be written in a language with a proof assistant -- we're not using that capability at this stage.

LeanPython is a tree-walking Python 3.12 interpreter written in Lean4. The end goal is to interpret the [leanSpec](https://github.com/leanEthereum/leanSpec) Ethereum consensus specification, making Python specs usable from within Lean4.

## Features

**Language**
- Full lexer with INDENT/DEDENT generation, f-strings, all numeric literal formats
- PEG parser covering expressions (18 precedence levels), statements, decorators, match/case
- Classes with single/multiple inheritance, MRO (C3 linearization), `__slots__`, `ClassVar`
- Operator overloading via dunder methods (`__add__`, `__eq__`, `__hash__`, etc.)
- `@classmethod`, `@staticmethod`, `@property`, `@dataclass`
- Exception handling (`try`/`except`/`finally`/`raise`, custom hierarchies)
- Generators and iterators (`yield`, `yield from`, `__iter__`, `__next__`)
- List, dict, set, and generator comprehensions
- Context managers (`with` statement)
- Module system with `import`, `from ... import`, relative imports, packages
- Tuple/list/dict unpacking, `*args`/`**kwargs`, starred assignments
- Walrus operator, slicing, chained comparisons
- Type annotations and `from __future__ import annotations`

**Built-in types**: `int` (arbitrary precision), `str`, `bytes`, `bytearray`, `bool`, `float`, `list`, `dict`, `tuple`, `set`, `frozenset`, `memoryview`, `None`

**Built-in functions**: `print`, `len`, `range`, `enumerate`, `zip`, `map`, `filter`, `sorted`, `reversed`, `any`, `all`, `sum`, `min`, `max`, `abs`, `pow`, `hash`, `id`, `type`, `isinstance`, `issubclass`, `getattr`, `setattr`, `hasattr`, `super`, `iter`, `next`, `repr`, `int`, `str`, `bytes`, `list`, `dict`, `tuple`, `set`, `frozenset`, `bool`, `float`, `hex`, `oct`, `bin`, `ord`, `chr`, and more

**Standard library modules**: `math`, `struct`, `io`, `bisect`, `base64`, `json`, `hashlib`, `hmac`, `secrets`, `sys`, `os`, `os.path`, `time`, `datetime`, `pathlib`, `logging`, `copy`, `functools`, `collections`, `pydantic`

## Getting started

Install [elan](https://github.com/leanprover/elan) (the Lean version manager):

```bash
curl https://elan.lean-lang.org/elan-init.sh -sSf | sh
```

Clone and build:

```bash
git clone https://github.com/zksecurity/leanPython.git
cd leanPython
lake build
```

The first build downloads the Lean toolchain and compiles everything -- this takes a few minutes.

Write a Python file and run it:

```bash
echo 'print("Hello, World!")' > hello.py
lake exe leanPython hello.py
```

Run the test suite:

```bash
lake test
```

## Project structure

```
LeanPython/
  Lexer/           -- Tokenizer (Python 3.12 tokens, INDENT/DEDENT)
  AST/             -- Python AST node types (mutual inductives)
  Parser/          -- PEG parser producing AST
  Interpreter/     -- Tree-walking interpreter (evalExpr + execStmt)
  Runtime/         -- Value types, operator dispatch, built-in functions
  Stdlib/          -- Standard library modules (math, json, hashlib, etc.)
Main.lean          -- CLI entry point
LeanPythonTest/    -- Test suite (lexer, parser, interpreter, stdlib)
```

See `CLAUDE.md` for the full file listing and `PLAN.md` for the development plan.

## Design

- **Tree-walking interpreter** -- simpler to implement, debug, and eventually prove properties about. Performance is secondary to correctness.
- **Immutable value semantics** where possible, leveraging Lean4's strengths. Python's mutable semantics are modeled explicitly with reference cells on a heap.
- **Arbitrary-precision integers** via Lean4's native `Nat`/`Int`.

## Contributing

The codebase is still rapidly changing and the author is pushing to main directly. Please wait for things to stabilize before opening PRs.
