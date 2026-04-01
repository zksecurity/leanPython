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
git clone --recurse-submodules https://github.com/zksecurity/leanPython.git
cd leanPython
lake build
```

If you already cloned without `--recurse-submodules`, run:

```bash
git submodule update --init --recursive
```

The first build downloads the Lean toolchain and compiles everything -- this takes a few minutes.

Write a Python file and run it:

```bash
echo 'print("Hello, World!")' > hello.py
lake exe leanPython hello.py
```

Run the tests:

```bash
lake test                    # Lean unit tests (#eval/#guard assertions)
./test_leanspec.sh           # E2E integration tests (19 tiers against real leanSpec files)
./test_leanspec_imports.sh   # leanSpec module import coverage (PASS/XFAIL/FAIL)
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

## Performance

LeanPython is roughly **6x faster** than CPython 3.12 on the leanSpec test suite, despite being a tree-walking interpreter. The compiled Lean4 binary avoids CPython's interpreter overhead and pydantic's heavy metaclass/validation machinery.

| Test | LeanPython | CPython 3.12 | Speedup |
|---|---|---|---|
| Constants + exceptions | 0.003s | 0.017s | ~6x |
| Uint64 arithmetic + codec | 0.009s | 0.092s | ~10x |
| Container serialize/deserialize | 0.013s | 0.088s | ~7x |
| Byte arrays | 0.014s | 0.091s | ~6x |
| SSZVector + SSZList | 0.021s | 0.097s | ~5x |
| **Total** | **0.060s** | **0.385s** | **~6x** |

Measured on 5 representative leanSpec tiers, each averaged over 3 runs.

## Design

- **Tree-walking interpreter** -- simpler to implement, debug, and eventually prove properties about.
- **Immutable value semantics** where possible, leveraging Lean4's strengths. Python's mutable semantics are modeled explicitly with reference cells on a heap.
- **Arbitrary-precision integers** via Lean4's native `Nat`/`Int`.

## Contributing

The codebase is still rapidly changing and the author is pushing to main directly. Please wait for things to stabilize before opening PRs.
