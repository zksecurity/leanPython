# LeanPython: Python Interpreter in Lean4

## Vision

Build a drop-in Python 3.12 interpreter written in Lean4, with the primary goal of
interpreting the [leanSpec](https://github.com/leanEthereum/leanSpec) Ethereum
consensus specification. The interpreter serves as a bridge: Python specs go in,
Lean4-verified execution comes out.

## Target Codebase Analysis

The leanSpec repository contains ~152 Python source files and ~176 test files
implementing the Lean Ethereum consensus protocol. The Python features required
are substantial but bounded:

### Python Features Required (by priority)

**Must Have (used pervasively in leanSpec):**
- Classes with single and multiple inheritance (`class State(Container)`)
- `__init__`, `__new__`, `__slots__`, `ClassVar`
- All arithmetic/comparison/bitwise dunder methods (operator overloading)
- `@classmethod`, `@staticmethod`, `@property`
- `@override`, `@singledispatch`, `@lru_cache`, `@dataclass(frozen=True, slots=True)`
- Exception handling (`try`/`except`/`finally`/`raise`/custom hierarchies)
- List, dict, set, and generator comprehensions
- Generators and iterators (`yield`, `__iter__`, `__next__`)
- `from __future__ import annotations` (PEP 563 deferred evaluation)
- Type annotations (runtime access via `__annotations__`, `get_type_hints()`)
- f-strings with nested expressions
- Tuple/list/dict unpacking (`*args`, `**kwargs`, starred assignments)
- Slicing (`a[1:3]`, `a[i*n:(i+1)*n]`)
- Walrus operator (`:=`)
- Context managers (`with` statement)
- Module system (`import`, `from ... import`, relative imports, packages)
- Built-in types: `int` (arbitrary precision), `str`, `bytes`, `bytearray`,
  `bool`, `float`, `list`, `dict`, `tuple`, `set`, `frozenset`, `memoryview`
- Built-in functions: `len`, `range`, `enumerate`, `zip`, `map`, `filter`,
  `sorted`, `reversed`, `any`, `all`, `sum`, `min`, `max`, `abs`, `pow`,
  `hash`, `id`, `type`, `isinstance`, `issubclass`, `getattr`, `setattr`,
  `hasattr`, `super`, `iter`, `next`, `print`, `repr`, `str`, `int`, `bytes`,
  `list`, `dict`, `tuple`, `set`, `frozenset`, `bool`, `float`
- `NamedTuple`
- `assert` statements
- String methods (`.removeprefix()`, `.startswith()`, `.hex()`, etc.)
- `int.from_bytes()`, `int.to_bytes()`

**Should Have (used in significant portions):**
- `async`/`await`, `asyncio.create_task()`, async context managers
- `Callable`, `Awaitable`, `Protocol` (structural typing at runtime)
- `TYPE_CHECKING` conditional imports
- `abc.ABC`, `abc.abstractmethod`
- `collections.defaultdict`, `collections.OrderedDict`, `collections.deque`
- `functools.singledispatch` with `.register`
- `functools.lru_cache`
- `itertools.accumulate`, `itertools.count`
- `struct.pack`, `struct.unpack`
- `hashlib.sha256`, `hashlib.shake_128`
- `hmac`
- `secrets.token_bytes`
- `math.ceil`, `math.log2`
- `bisect.bisect_left`
- `io.BytesIO`
- `pathlib.Path`
- `logging` module
- `json`, `yaml` serialization
- `base64` encoding
- `ipaddress` module
- `signal` handling
- `threading.Lock`

**Nice to Have (infrastructure, not core spec):**
- `sqlite3`
- `httpx`, `aiohttp` (HTTP client/server)
- `aioquic` (QUIC protocol)
- `numpy`, `numba` (numerical/JIT)
- `pydantic` (validation framework)
- `prometheus_client` (metrics)
- `argparse` (CLI)

### What Pydantic Means for Us

Pydantic is the most challenging dependency. Every container in leanSpec is a
Pydantic `BaseModel`. Two strategies:

1. **Pydantic-in-Lean4**: Reimplement the subset of Pydantic used (model
   definition, `model_copy`, `model_fields`, `model_dump`, field validators,
   frozen models). This is the target approach.
2. **FFI to Python Pydantic**: Call out to CPython for Pydantic operations. This
   is the escape hatch if (1) proves too costly.

The subset of Pydantic actually used is narrow:
- `BaseModel` with `model_config = ConfigDict(frozen=True, extra="forbid")`
- `model_copy(update={...})`
- `model_fields` (field introspection)
- `model_dump(mode="json")`
- `field_validator`, `field_serializer`, `model_validator`
- `__get_pydantic_core_schema__` for custom types

---

## Architecture

```
LeanPython/
  src/
    LeanPython/
      Lexer/           -- Tokenizer (Python 3.12 tokens)
      Parser/          -- PEG parser producing AST
      AST/             -- Python AST node types
      Interpreter/     -- Tree-walking interpreter
        Core/          -- Expression/statement evaluation
        Object/        -- Python object model
        Module/        -- Import system
        Builtins/      -- Built-in functions and types
      Runtime/         -- Runtime support
        Types/         -- int, str, bytes, list, dict, ...
        Exceptions/    -- Exception hierarchy
        StdLib/        -- Standard library modules
        Pydantic/      -- Pydantic compatibility layer
      FFI/             -- Optional CPython FFI bridge
    Tests/             -- Lean4 test infrastructure
  lakefile.lean        -- Build configuration
  lean-toolchain       -- Lean4 version pin
```

### Design Decisions

- **Tree-walking interpreter** (not bytecode VM). Simpler to implement, debug,
  and prove properties about. Performance is secondary to correctness.
- **Immutable value semantics** where possible, leveraging Lean4's strengths.
  Python's mutable semantics are modeled explicitly with reference cells.
- **Arbitrary-precision integers** use Lean4's native `Nat`/`Int`.
- **Strings** use Lean4's native `String` with Python-compatible methods.
- **Bytes** use `ByteArray`.

---

## Phases

### Phase 0: Project Scaffolding

**Goal**: Set up the Lean4 project, CI, and basic infrastructure.

**Deliverables:**
- `lakefile.lean` with project structure
- `lean-toolchain` pinned to stable Lean4
- CI pipeline (GitHub Actions) running `lake build` and `lake test`
- Basic project structure with placeholder modules

**Testing:**
- `lake build` succeeds
- `lake test` runs (even if no tests yet)
- CI green on push

---

### Phase 1: Lexer

**Goal**: Tokenize Python 3.12 source code into a token stream.

**Scope:**
- All Python 3.12 token types (identifiers, keywords, literals, operators,
  delimiters, INDENT/DEDENT, NEWLINE, ENDMARKER)
- String literals: single/double/triple-quoted, f-strings, b-strings, r-strings
- Numeric literals: integers (dec, hex, oct, bin), floats, complex
- Continuation lines (explicit `\` and implicit inside brackets)
- Comments (discarded)
- Encoding detection (UTF-8 default)
- INDENT/DEDENT generation from leading whitespace

**Key types:**
```
inductive TokenKind where
  | name : String -> TokenKind
  | integer : Int -> TokenKind
  | float_ : Float -> TokenKind
  | string : String -> TokenKind
  | fstring : List FStringPart -> TokenKind
  | bytes : ByteArray -> TokenKind
  | keyword : Keyword -> TokenKind
  | operator : Operator -> TokenKind
  | delimiter : Delimiter -> TokenKind
  | indent | dedent | newline | endmarker
  | comment : String -> TokenKind

structure Token where
  kind : TokenKind
  span : SourceSpan
```

**Testing strategy:**
- **Unit tests**: Tokenize individual token types and verify output.
  - Every keyword, operator, delimiter
  - String edge cases: escapes, raw strings, multiline, f-string nesting
  - Numeric edge cases: `0x`, `0o`, `0b`, underscores, leading zeros
  - INDENT/DEDENT: tabs, spaces, mixed (error), nested blocks
- **Round-trip tests**: Tokenize then reconstruct source; verify equivalence.
- **CPython conformance**: Tokenize files from `references/cpython/Lib/test/`
  and compare token streams against CPython's `tokenize` module output.
- **leanSpec smoke test**: Successfully tokenize all 152 source files in
  `references/leanSpec/src/lean_spec/` without errors.

**Acceptance criteria:**
- All 152 leanSpec source files tokenize without errors.
- Token stream matches CPython's `tokenize` output for a curated test suite.

---

### Phase 2: Parser

**Goal**: Parse the token stream into a Python 3.12 AST.

**Scope:**
- PEG parser following `references/cpython/Grammar/python.gram`
- Full expression grammar (atoms, primaries, unary/binary/ternary ops,
  comprehensions, lambda, yield, named expressions)
- Full statement grammar (simple statements, compound statements)
- Decorators
- Type annotations (stored but not evaluated at parse time)
- `match`/`case` (structural pattern matching) -- parse only, interpret later
- Error recovery with source locations

**Key AST types** (mirroring CPython's `ast` module):
```
inductive Expr where
  | name : String -> Expr
  | constant : Constant -> Expr
  | binOp : Expr -> BinOp -> Expr -> Expr
  | unaryOp : UnaryOp -> Expr -> Expr
  | boolOp : BoolOp -> List Expr -> Expr
  | compare : Expr -> List (CmpOp × Expr) -> Expr
  | call : Expr -> List Expr -> List Keyword -> Expr
  | attribute : Expr -> String -> Expr
  | subscript : Expr -> Expr -> Expr
  | starred : Expr -> Expr
  | ifExp : Expr -> Expr -> Expr -> Expr
  | lambda_ : Arguments -> Expr -> Expr
  | listComp : Expr -> List Comprehension -> Expr
  | dictComp : Expr -> Expr -> List Comprehension -> Expr
  | setComp : Expr -> List Comprehension -> Expr
  | generatorExp : Expr -> List Comprehension -> Expr
  | await_ : Expr -> Expr
  | yield_ : Option Expr -> Expr
  | yieldFrom : Expr -> Expr
  | fstring : List FStringPart -> Expr
  | namedExpr : String -> Expr -> Expr
  | slice : Option Expr -> Option Expr -> Option Expr -> Expr
  | tuple_ : List Expr -> Expr
  | list_ : List Expr -> Expr
  | set_ : List Expr -> Expr
  | dict_ : List (Option Expr × Expr) -> Expr
  -- ... etc

inductive Stmt where
  | expr : Expr -> Stmt
  | assign : List Expr -> Expr -> Stmt
  | augAssign : Expr -> BinOp -> Expr -> Stmt
  | annAssign : Expr -> Expr -> Option Expr -> Stmt
  | return_ : Option Expr -> Stmt
  | delete : List Expr -> Stmt
  | raise_ : Option Expr -> Option Expr -> Stmt
  | assert_ : Expr -> Option Expr -> Stmt
  | import_ : List Alias -> Stmt
  | importFrom : Option String -> List Alias -> Option Int -> Stmt
  | if_ : Expr -> List Stmt -> List Stmt -> Stmt
  | while_ : Expr -> List Stmt -> List Stmt -> Stmt
  | for_ : Expr -> Expr -> List Stmt -> List Stmt -> Stmt
  | with_ : List WithItem -> List Stmt -> Stmt
  | try_ : List Stmt -> List ExceptHandler -> List Stmt -> List Stmt -> Stmt
  | functionDef : FunctionDef -> Stmt
  | classDef : ClassDef -> Stmt
  | global : List String -> Stmt
  | nonlocal : List String -> Stmt
  | pass_ | break_ | continue_ : Stmt
  | asyncFor : Expr -> Expr -> List Stmt -> List Stmt -> Stmt
  | asyncWith : List WithItem -> List Stmt -> Stmt
  | asyncFunctionDef : FunctionDef -> Stmt
  | match_ : Expr -> List MatchCase -> Stmt
  -- ... etc
```

**Testing strategy:**
- **Unit tests**: Parse individual constructs and verify AST structure.
  - Every statement type, every expression type
  - Operator precedence and associativity
  - Decorator chains
  - Complex comprehensions with nested `for`/`if`
  - f-string parsing with nested expressions
  - Star expressions, unpacking assignments
  - `match`/`case` patterns
- **CPython conformance**: Parse files and compare AST against `ast.dump()`
  output from CPython (modulo field naming).
- **Negative tests**: Verify parse errors for invalid syntax with clear messages.
- **leanSpec smoke test**: Parse all 152 source files without errors.

**Acceptance criteria:**
- All 152 leanSpec source files parse without errors.
- AST structure matches CPython's `ast.parse()` for a curated test suite.

---

### Phase 3: Core Interpreter -- Expressions and Primitive Types

**Goal**: Evaluate Python expressions with primitive types.

**Scope:**
- Python object model foundation:
  - `PyObject` base type with `__class__`, `__dict__`
  - Type/class hierarchy: `object`, `type`
  - Attribute lookup (instance -> class -> MRO)
  - Descriptor protocol (`__get__`, `__set__`, `__delete__`)
- Primitive types with full method sets:
  - `int` (arbitrary precision, all operators, `to_bytes`, `from_bytes`, `bit_length`)
  - `bool` (subclass of `int`)
  - `float` (IEEE 754 double, all operators)
  - `str` (full method set: `format`, `join`, `split`, `strip`, `replace`,
    `startswith`, `endswith`, `removeprefix`, `encode`, `hex`, f-string formatting)
  - `bytes` / `bytearray` (full method set: `hex`, `fromhex`, concatenation, slicing)
  - `None`, `Ellipsis`, `NotImplemented`
- Container types:
  - `list` (append, extend, insert, pop, remove, sort, reverse, slicing, `*` repeat)
  - `tuple` (indexing, slicing, unpacking)
  - `dict` (get, setdefault, items, keys, values, update, `|` merge, comprehension)
  - `set` / `frozenset` (add, remove, union, intersection, difference, symmetric_difference)
- Expression evaluation:
  - All binary operators with type dispatch
  - All unary operators
  - Comparison chaining (`a < b <= c`)
  - Boolean operators (`and`, `or`, `not`) with short-circuit
  - Conditional expression (`x if cond else y`)
  - Attribute access and method calls
  - Subscript and slicing
  - Starred unpacking
  - Walrus operator
  - All comprehension types
- Variable scoping (LEGB: Local, Enclosing, Global, Builtin)
- Assignment (simple, augmented, annotated, unpacking)
- `del` statement

**Testing strategy:**
- **Unit tests per type**: Exercise every method and operator.
  - `int`: arithmetic, bitwise, comparison, `to_bytes`/`from_bytes`, edge cases
    (very large numbers, negative, zero)
  - `str`: all methods used in leanSpec, f-string formatting, encoding
  - `bytes`: slicing, `hex()`, `fromhex()`, concatenation
  - `list`: mutations, slicing, comprehensions
  - `dict`: comprehensions, `|` operator, iteration order
  - `set`: set operations, membership testing
- **Operator dispatch tests**: Mixed-type operations, `NotImplemented` fallback,
  reflected operators (`__radd__` etc.)
- **Scoping tests**: Closures, nested functions, `global`/`nonlocal`.
- **leanSpec micro-tests**: Extract small expressions from leanSpec and verify
  evaluation matches CPython. Examples:
  - `2**cls.BITS - 1` for various BITS values
  - `int.from_bytes(data, "little")`
  - List/dict comprehensions from `hash.py`
  - Slice operations from `state.py`

**Acceptance criteria:**
- All primitive type operations produce results matching CPython.
- Comprehensions, slicing, and unpacking work correctly.
- Scoping rules match CPython behavior.

---

### Phase 4: Control Flow and Functions

**Goal**: Execute function definitions, calls, and all control flow.

**Scope:**
- Function definitions (positional, keyword, default, `*args`, `**kwargs`)
- Function calls with full argument binding
- `return`, `yield`, `yield from`
- Generator protocol (`__iter__`, `__next__`, `StopIteration`)
- `if`/`elif`/`else`
- `while` with `break`/`continue`/`else`
- `for` with `break`/`continue`/`else`
- `try`/`except`/`else`/`finally`
- `raise` with chaining (`raise X from Y`)
- `assert`
- `with` statement (context manager protocol)
- `del` statement
- `pass`, `break`, `continue`
- Lambda expressions
- Closures and free variable capture
- `global` and `nonlocal` declarations

**Testing strategy:**
- **Function call tests**:
  - Positional, keyword, default, `*args`, `**kwargs` in all combinations
  - Argument binding edge cases (too few, too many, duplicate keywords)
- **Generator tests**:
  - Simple generators, `yield from`, generator expressions
  - `send()`, `throw()`, `close()` on generators
  - Generator as iterator in `for` loops
- **Exception tests**:
  - `try`/`except` with type matching
  - Exception hierarchy (`except BaseException`, `except Exception`)
  - `except ... as e` binding
  - `finally` always runs (even with `return`/`break`)
  - `raise from` chaining
  - `assert` with and without message
- **Context manager tests**:
  - `__enter__`/`__exit__` protocol
  - Exception propagation through `__exit__`
  - Multiple `with` items
- **leanSpec function tests**: Extract functions from leanSpec and execute.
  - `BaseUint.__new__` with validation
  - `Fp.__add__`, `Fp.__mul__`, `Fp.inverse` (field arithmetic)
  - Generator methods like `Store.ancestors`
  - `State.process_slots` loop

**Acceptance criteria:**
- All control flow constructs work correctly.
- Generators are fully functional.
- Exception handling matches CPython semantics.
- Context managers work with proper cleanup.

---

### Phase 5: Object Model -- Classes, Inheritance, Descriptors

**Goal**: Full Python class system.

**Scope:**
- Class definition with decorators
- Single and multiple inheritance
- MRO computation (C3 linearization)
- `super()` with implicit and explicit forms
- Instance creation (`__new__` + `__init__`)
- Attribute access through MRO
- Descriptor protocol (`__get__`, `__set__`, `__delete__`, `__set_name__`)
- `@classmethod`, `@staticmethod`, `@property`
- `@dataclass` with `frozen=True`, `slots=True`
- Metaclasses (`type` as default metaclass)
- `__slots__`
- `isinstance()`, `issubclass()`
- `__repr__`, `__str__`, `__hash__`, `__eq__`
- `__getattr__`, `__getattribute__`, `__setattr__`, `__delattr__`
- `__contains__`, `__len__`, `__iter__`, `__getitem__`, `__setitem__`
- `__enter__`, `__exit__`
- `__init_subclass__`
- Class variables vs instance variables
- `ClassVar` type hint (runtime semantics)
- `type()` for dynamic class creation

**Testing strategy:**
- **Inheritance tests**:
  - Diamond inheritance with C3 MRO
  - `super()` in single and multiple inheritance
  - Method resolution through MRO chain
  - `class Uint64(BaseUint)` with `BITS = 64` -- from leanSpec
  - `class Container(SSZModel)` -- from leanSpec
- **Descriptor tests**:
  - `@property` getter/setter/deleter
  - `@classmethod` and `@staticmethod`
  - Custom descriptors
- **Operator overloading tests**:
  - All dunder methods from `BaseUint` (add, sub, mul, etc.)
  - `Fp` field arithmetic through operator overloading
  - Comparison operators, hashing
- **Dataclass tests**:
  - `@dataclass(frozen=True, slots=True)`
  - Field defaults and `field()` factory
  - `__post_init__`
- **leanSpec class tests**:
  - Construct `Uint64(42)`, verify all operators
  - Construct `Fp(value=7)`, verify field arithmetic
  - Construct `Bytes32.zero()`, verify methods
  - Construct `Container` subclasses with field access

**Acceptance criteria:**
- C3 MRO matches CPython for all leanSpec class hierarchies.
- Operator overloading works for `BaseUint`, `Fp`, and `BaseBytes`.
- `@dataclass` produces correct frozen, slotted classes.
- `super()` works correctly in all leanSpec usage patterns.

---

### Phase 6: Module System

**Goal**: Import and execute Python modules and packages.

**Scope:**
- `import module`
- `from module import name`
- `from module import *` (with `__all__`)
- Relative imports (`from . import x`, `from ..sub import y`)
- Package `__init__.py` execution
- `sys.path` search and module resolution
- Module caching (`sys.modules`)
- `__name__`, `__file__`, `__package__` attributes
- `from __future__ import annotations` (disable annotation evaluation)
- `TYPE_CHECKING` constant (always `False` at runtime)
- Circular import handling

**Testing strategy:**
- **Import resolution tests**:
  - Absolute imports
  - Relative imports at various depths
  - Package `__init__.py` re-exports
  - `from __future__ import annotations`
- **Module execution tests**:
  - Module-level code execution
  - Module-level constants and `Final` annotation
  - `__all__` filtering for `import *`
- **Circular import tests**:
  - Detect and handle circular imports gracefully
  - `TYPE_CHECKING` conditional imports don't trigger at runtime
- **leanSpec import tests**:
  - Import `lean_spec.types.uint` and verify `Uint64` is accessible
  - Import `lean_spec.types.container` and verify `Container` is accessible
  - Import `lean_spec.subspecs.ssz.hash` and verify `hash_tree_root` works
  - Import chain: `state.py` -> `checkpoint.py` -> `slot.py` -> `uint.py`
  - Full leanSpec import graph resolves without errors

**Acceptance criteria:**
- All leanSpec import statements resolve correctly.
- Relative imports work within the leanSpec package hierarchy.
- Module-level code executes in correct order.

---

### Phase 7: Standard Library

**Goal**: Implement the standard library modules used by leanSpec.

This phase is split into sub-phases, each adding a group of related modules.

#### Phase 7a: Core Utilities
- `functools`: `singledispatch` (with `.register`), `lru_cache`, `reduce`
- `itertools`: `accumulate`, `count`, `chain`
- `collections`: `defaultdict`, `OrderedDict`, `deque`
- `collections.abc`: `Iterable`, `Iterator`, `Mapping`, `Sequence`, `Set`,
  `MutableMapping`, `MutableSequence`, `MutableSet`, `Callable`, `Hashable`
- `typing`: `Any`, `ClassVar`, `Final`, `Self`, `Literal`, `TypeAlias`,
  `SupportsInt`, `SupportsIndex`, `IO`, `Protocol`, `runtime_checkable`,
  `TYPE_CHECKING`, `override`, `NoReturn`, `NamedTuple`, `get_type_hints`
- `abc`: `ABC`, `abstractmethod`
- `dataclasses`: `dataclass`, `field`, `fields`
- `enum`: `Enum`, `IntEnum`
- `math`: `ceil`, `log2`, `floor`, `sqrt`, `inf`, `nan`
- `copy`: `copy`, `deepcopy`
- `operator`: `itemgetter`, `attrgetter`

#### Phase 7b: Data Handling
- `struct`: `pack`, `unpack`, `calcsize`
- `io`: `BytesIO`, `StringIO`
- `json`: `dumps`, `loads`
- `base64`: `b64encode`, `b64decode`, `b16encode`, `b16decode`
- `bisect`: `bisect_left`, `bisect_right`, `insort`
- `re`: basic regex (if needed)

#### Phase 7c: Cryptography and Hashing
- `hashlib`: `sha256`, `shake_128`, `new`
- `hmac`: `new`, `digest`
- `secrets`: `token_bytes`, `randbelow`

#### Phase 7d: System and OS
- `os`: `environ`, `path`, `getcwd`
- `sys`: `path`, `modules`, `argv`, `stdout`, `stderr`, `exit`
- `pathlib`: `Path`
- `logging`: `Logger`, `getLogger`, `basicConfig`, handlers
- `time`: `time`, `monotonic`, `sleep`
- `datetime`: `datetime`, `timedelta`, `timezone`
- `tempfile`: `NamedTemporaryFile`, `mkdtemp`
- `signal`: `signal`, `SIGINT`, `SIGTERM`
- `threading`: `Lock`, `RLock`

**Testing strategy per sub-phase:**
- **Unit tests**: Each function/class tested independently against CPython.
- **Conformance tests**: Run the same test with both CPython and LeanPython,
  compare output.
- **leanSpec integration**: After each sub-phase, attempt to import and run
  progressively more of leanSpec.

**Acceptance criteria:**
- `functools.singledispatch` works with leanSpec's `hash_tree_root` pattern.
- `hashlib.sha256` produces correct digests.
- `struct.pack/unpack` handles little-endian byte encoding.
- All standard library functions used in leanSpec produce CPython-identical results.

---

### Phase 8: Pydantic Compatibility Layer

**Goal**: Implement the subset of Pydantic used by leanSpec.

This is the most challenging phase. Pydantic is deeply integrated into leanSpec --
every container, every type, every validation path uses it.

**Scope:**
- `BaseModel` with:
  - Field declaration via class annotations
  - `model_config = ConfigDict(frozen=True, extra="forbid", strict=True)`
  - `model_copy(update={...})` (immutable copy with field updates)
  - `model_fields` (field name -> FieldInfo mapping)
  - `model_dump(mode="json")` (serialization to dict)
  - `__init__` validation (type checking, coercion)
  - `__eq__`, `__hash__`, `__repr__` generation
  - `__iter__` over field values
- Field validators:
  - `@field_validator("field_name", mode="before"|"after")`
  - `@model_validator(mode="before"|"after")`
  - `@field_serializer("field_name")`
- Custom type schemas:
  - `__get_pydantic_core_schema__` hook
  - `core_schema.union_schema`, `core_schema.is_instance_schema`,
    `core_schema.chain_schema`, `core_schema.int_schema`,
    `core_schema.no_info_plain_validator_function`,
    `core_schema.plain_serializer_function_ser_schema`
- `ConfigDict` options: `frozen`, `extra`, `strict`, `populate_by_name`
- Alias generation (CamelCase via `alias_generator`)

**Testing strategy:**
- **Model definition tests**: Define models, verify field access, immutability,
  extra field rejection.
- **Validation tests**: Valid/invalid inputs, type coercion, custom validators.
- **Serialization tests**: `model_dump` output matches CPython/Pydantic output.
- **model_copy tests**: Verify immutable updates produce correct new instances.
- **leanSpec container tests**:
  - Define `Config`, `Checkpoint`, `Slot`, `ValidatorIndex` containers
  - Construct, validate, serialize
  - `model_copy(update={...})` on `State` (as used in `process_slots`)
  - `model_fields` iteration (as used in `Container.serialize`)
  - `__get_pydantic_core_schema__` for `BaseUint` types

**Acceptance criteria:**
- All leanSpec container types can be defined and instantiated.
- `model_copy` produces correct immutable updates.
- `model_fields` returns correct field metadata.
- Validation rejects invalid inputs as Pydantic does.

---

### Phase 9: leanSpec Integration

**Goal**: Execute the core leanSpec consensus specification end-to-end.

**Scope (incremental):**

#### Phase 9a: Types
- Import and use all SSZ types: `Uint8`..`Uint64`, `Boolean`, `Bytes32`, `Fp`
- Import and use collection types: `SSZList`, `SSZVector`, `BaseBitlist`
- Import and use containers: `Container` base and all field types
- SSZ serialization/deserialization round-trip

#### Phase 9b: SSZ Merkleization
- `hash_tree_root` for all types via `singledispatch`
- `merkleize`, `mix_in_length`, `pack_bytes`, `pack_bits`
- Correct Merkle roots matching CPython output

#### Phase 9c: Consensus Containers
- `State`, `Block`, `BlockHeader`, `BlockBody`
- `Checkpoint`, `Slot`, `ValidatorIndex`
- `Attestation`, `AggregatedAttestation`, `AggregationBits`
- All container methods

#### Phase 9d: State Transition
- `State.process_slots` (slot advancement)
- `State.process_block_header` (block header validation)
- `State.process_attestations` (justification/finalization)
- `State.state_transition` (full STF)
- `State.build_block` (block building)

#### Phase 9e: Fork Choice
- `Store` with block/state management
- `Store.ancestors` generator
- Fork choice algorithm

#### Phase 9f: Cryptographic Subspecs
- XMSS signature scheme
- Poseidon2 hash function
- KoalaBear field arithmetic (already covered by `Fp` tests)

**Testing strategy:**
- **Golden test vectors**: For each sub-phase, generate test vectors from
  CPython/leanSpec and verify LeanPython produces identical results.
- **State transition tests**: Run leanSpec's own `tests/lean_spec/` test suite
  through LeanPython and compare results.
- **Fixture generation**: Run `tests/consensus/` fillers and verify the generated
  JSON fixtures match CPython output byte-for-byte.
- **Hash comparison**: Every `hash_tree_root` call produces the same `Bytes32`
  as CPython.
- **End-to-end**: Process a sequence of blocks through `state_transition` and
  verify final state matches CPython.

**Acceptance criteria:**
- All leanSpec unit tests pass when run through LeanPython.
- Generated consensus fixtures match CPython output.
- State transition for multi-block sequences produces identical states.

---

### Phase 10: Async Runtime (Optional)

**Goal**: Support `async`/`await` for leanSpec's networking and node code.

**Scope:**
- Event loop (basic `asyncio` subset)
- `async def`, `await`, `async for`, `async with`
- `asyncio.create_task`, `asyncio.gather`, `asyncio.sleep`
- `asyncio.Queue`, `asyncio.Event`
- Task cancellation

This phase is optional for the core consensus spec interpretation but required
for the full node implementation (networking, API, validator service).

**Testing strategy:**
- **Coroutine tests**: Basic async function execution, awaiting results.
- **Task tests**: Concurrent task execution, cancellation.
- **leanSpec node tests**: Run the simplified node startup sequence.

---

## Testing Infrastructure

### Test Harness

The test infrastructure supports three modes:

1. **Lean4 Unit Tests** (`lake test`): Native Lean4 tests for interpreter
   internals (lexer, parser, evaluator).

2. **Python Conformance Tests**: A test runner that executes `.py` test files
   through both CPython and LeanPython, comparing outputs. Directory structure:
   ```
   tests/
     conformance/
       expressions/     -- arithmetic, comparisons, boolean ops
       statements/      -- assignments, control flow, imports
       functions/       -- calls, generators, closures
       classes/         -- inheritance, descriptors, operators
       stdlib/          -- standard library module tests
       pydantic/        -- pydantic compatibility tests
       leanspec/        -- extracted leanSpec test cases
   ```

3. **leanSpec Test Suite**: Run leanSpec's own `tests/lean_spec/` tests directly
   through LeanPython.

### Conformance Testing Process

For each phase, conformance tests follow this pattern:

```
1. Write Python test file (test_foo.py)
2. Run with CPython -> capture output/state
3. Run with LeanPython -> capture output/state
4. Assert outputs match
```

For deterministic tests (no randomness, no I/O), outputs must match exactly.
For non-deterministic tests, structural equivalence is checked.

### Continuous Integration

- Every PR runs: `lake build`, `lake test`, conformance tests for completed phases.
- Nightly: Full leanSpec test suite execution (longer running).
- Coverage tracking: Which leanSpec files can be fully interpreted.

---

## Progress Tracking

| Phase | Description | Status |
|-------|-------------|--------|
| 0 | Project scaffolding | Done |
| 1 | Lexer | Done |
| 2 | Parser | Done |
| 3 | Core interpreter (expressions, types) | Done |
| 4 | Control flow and functions | In progress |
| 5 | Object model (classes, inheritance) | Not started |
| 6 | Module system | Not started |
| 7a | Stdlib: core utilities | Not started |
| 7b | Stdlib: data handling | Not started |
| 7c | Stdlib: cryptography | Not started |
| 7d | Stdlib: system/OS | Not started |
| 8 | Pydantic compatibility | Not started |
| 9a | leanSpec: types | Not started |
| 9b | leanSpec: SSZ merkleization | Not started |
| 9c | leanSpec: consensus containers | Not started |
| 9d | leanSpec: state transition | Not started |
| 9e | leanSpec: fork choice | Not started |
| 9f | leanSpec: cryptographic subspecs | Not started |
| 10 | Async runtime (optional) | Not started |

---

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| Pydantic complexity | High | Start with minimal subset; FFI escape hatch |
| Python semantics edge cases | Medium | Extensive conformance testing against CPython |
| Performance (tree-walking) | Medium | Acceptable for spec interpretation; optimize later if needed |
| Lean4 library gaps | Low | Use FFI for truly missing functionality |
| leanSpec evolution | Medium | Pin to specific commit; track upstream changes |

---

## Non-Goals

- Full CPython compatibility (only the subset used by leanSpec)
- Performance parity with CPython
- C extension module support
- `eval()`/`exec()` (not used in leanSpec)
- Python 2 compatibility
- REPL/interactive mode (initially)
- Formal verification of the interpreter (future work)
- The networking/API/storage infrastructure in leanSpec (Phase 10 scope)
