import LeanPython

open LeanPython.Lexer

/-- Helper: extract token kinds from a source string. -/
def tokenKinds (source : String) : Option (List TokenKind) :=
  match tokenize source with
  | .ok tokens => some (tokens.toList.map Token.kind)
  | .error _ => none

-- ============================================================
-- Basic smoke tests
-- ============================================================

-- Empty source → just endmarker
#guard tokenKinds "" == some [.endmarker]

-- Single identifier
#guard tokenKinds "x" == some [.name "x", .newline, .endmarker]

-- Identifier with trailing newline
#guard tokenKinds "x\n" == some [.name "x", .newline, .endmarker]

-- Multiple identifiers
#guard tokenKinds "x y" == some [.name "x", .name "y", .newline, .endmarker]

-- ============================================================
-- Keywords
-- ============================================================

#guard tokenKinds "if" == some [.keyword .if_, .newline, .endmarker]
#guard tokenKinds "def" == some [.keyword .def_, .newline, .endmarker]
#guard tokenKinds "class" == some [.keyword .class_, .newline, .endmarker]
#guard tokenKinds "return" == some [.keyword .return_, .newline, .endmarker]
#guard tokenKinds "True" == some [.keyword .true_, .newline, .endmarker]
#guard tokenKinds "False" == some [.keyword .false_, .newline, .endmarker]
#guard tokenKinds "None" == some [.keyword .none_, .newline, .endmarker]
#guard tokenKinds "and" == some [.keyword .and_, .newline, .endmarker]
#guard tokenKinds "or" == some [.keyword .or_, .newline, .endmarker]
#guard tokenKinds "not" == some [.keyword .not_, .newline, .endmarker]
#guard tokenKinds "import" == some [.keyword .import_, .newline, .endmarker]
#guard tokenKinds "from" == some [.keyword .from_, .newline, .endmarker]
#guard tokenKinds "for" == some [.keyword .for_, .newline, .endmarker]
#guard tokenKinds "while" == some [.keyword .while_, .newline, .endmarker]
#guard tokenKinds "with" == some [.keyword .with_, .newline, .endmarker]
#guard tokenKinds "as" == some [.keyword .as_, .newline, .endmarker]
#guard tokenKinds "try" == some [.keyword .try_, .newline, .endmarker]
#guard tokenKinds "except" == some [.keyword .except_, .newline, .endmarker]
#guard tokenKinds "finally" == some [.keyword .finally_, .newline, .endmarker]
#guard tokenKinds "raise" == some [.keyword .raise, .newline, .endmarker]
#guard tokenKinds "yield" == some [.keyword .yield_, .newline, .endmarker]
#guard tokenKinds "lambda" == some [.keyword .lambda_, .newline, .endmarker]
#guard tokenKinds "global" == some [.keyword .global, .newline, .endmarker]
#guard tokenKinds "nonlocal" == some [.keyword .nonlocal, .newline, .endmarker]
#guard tokenKinds "del" == some [.keyword .del, .newline, .endmarker]
#guard tokenKinds "pass" == some [.keyword .pass_, .newline, .endmarker]
#guard tokenKinds "break" == some [.keyword .break_, .newline, .endmarker]
#guard tokenKinds "continue" == some [.keyword .continue_, .newline, .endmarker]
#guard tokenKinds "in" == some [.keyword .in_, .newline, .endmarker]
#guard tokenKinds "is" == some [.keyword .is_, .newline, .endmarker]
#guard tokenKinds "elif" == some [.keyword .elif, .newline, .endmarker]
#guard tokenKinds "else" == some [.keyword .else_, .newline, .endmarker]
#guard tokenKinds "assert" == some [.keyword .assert_, .newline, .endmarker]
#guard tokenKinds "async" == some [.keyword .async, .newline, .endmarker]
#guard tokenKinds "await" == some [.keyword .await, .newline, .endmarker]

-- Not keywords
#guard tokenKinds "iff" == some [.name "iff", .newline, .endmarker]
#guard tokenKinds "define" == some [.name "define", .newline, .endmarker]
#guard tokenKinds "Class" == some [.name "Class", .newline, .endmarker]

-- ============================================================
-- Operators
-- ============================================================

#guard tokenKinds "+" == some [.operator .plus, .newline, .endmarker]
#guard tokenKinds "-" == some [.operator .minus, .newline, .endmarker]
#guard tokenKinds "*" == some [.operator .star, .newline, .endmarker]
#guard tokenKinds "/" == some [.operator .slash, .newline, .endmarker]
#guard tokenKinds "//" == some [.operator .doubleSlash, .newline, .endmarker]
#guard tokenKinds "%" == some [.operator .percent, .newline, .endmarker]
#guard tokenKinds "**" == some [.operator .doubleStar, .newline, .endmarker]
#guard tokenKinds "@" == some [.operator .at, .newline, .endmarker]
#guard tokenKinds "&" == some [.operator .amper, .newline, .endmarker]
#guard tokenKinds "|" == some [.operator .vbar, .newline, .endmarker]
#guard tokenKinds "^" == some [.operator .circumflex, .newline, .endmarker]
#guard tokenKinds "~" == some [.operator .tilde, .newline, .endmarker]
#guard tokenKinds "<<" == some [.operator .leftShift, .newline, .endmarker]
#guard tokenKinds ">>" == some [.operator .rightShift, .newline, .endmarker]
#guard tokenKinds "==" == some [.operator .eqEqual, .newline, .endmarker]
#guard tokenKinds "!=" == some [.operator .notEqual, .newline, .endmarker]
#guard tokenKinds "<" == some [.operator .less, .newline, .endmarker]
#guard tokenKinds ">" == some [.operator .greater, .newline, .endmarker]
#guard tokenKinds "<=" == some [.operator .lessEqual, .newline, .endmarker]
#guard tokenKinds ">=" == some [.operator .greaterEqual, .newline, .endmarker]
#guard tokenKinds "=" == some [.operator .equal, .newline, .endmarker]
#guard tokenKinds "+=" == some [.operator .plusEqual, .newline, .endmarker]
#guard tokenKinds "-=" == some [.operator .minEqual, .newline, .endmarker]
#guard tokenKinds "*=" == some [.operator .starEqual, .newline, .endmarker]
#guard tokenKinds "/=" == some [.operator .slashEqual, .newline, .endmarker]
#guard tokenKinds "//=" == some [.operator .doubleSlashEqual, .newline, .endmarker]
#guard tokenKinds "%=" == some [.operator .percentEqual, .newline, .endmarker]
#guard tokenKinds "**=" == some [.operator .doubleStarEqual, .newline, .endmarker]
#guard tokenKinds "@=" == some [.operator .atEqual, .newline, .endmarker]
#guard tokenKinds "&=" == some [.operator .amperEqual, .newline, .endmarker]
#guard tokenKinds "|=" == some [.operator .vbarEqual, .newline, .endmarker]
#guard tokenKinds "^=" == some [.operator .circumflexEqual, .newline, .endmarker]
#guard tokenKinds "<<=" == some [.operator .leftShiftEqual, .newline, .endmarker]
#guard tokenKinds ">>=" == some [.operator .rightShiftEqual, .newline, .endmarker]
#guard tokenKinds "->" == some [.operator .rarrow, .newline, .endmarker]
#guard tokenKinds ":=" == some [.operator .colonEqual, .newline, .endmarker]

-- ============================================================
-- Delimiters
-- ============================================================

#guard tokenKinds "(" == some [.delimiter .lpar, .newline, .endmarker]
#guard tokenKinds ")" == some [.delimiter .rpar, .newline, .endmarker]
#guard tokenKinds "[" == some [.delimiter .lsqb, .newline, .endmarker]
#guard tokenKinds "]" == some [.delimiter .rsqb, .newline, .endmarker]
#guard tokenKinds "{" == some [.delimiter .lbrace, .newline, .endmarker]
#guard tokenKinds "}" == some [.delimiter .rbrace, .newline, .endmarker]
#guard tokenKinds ":" == some [.delimiter .colon, .newline, .endmarker]
#guard tokenKinds "," == some [.delimiter .comma, .newline, .endmarker]
#guard tokenKinds ";" == some [.delimiter .semi, .newline, .endmarker]
#guard tokenKinds "." == some [.delimiter .dot, .newline, .endmarker]
#guard tokenKinds "..." == some [.delimiter .ellipsis, .newline, .endmarker]

-- ============================================================
-- Integers
-- ============================================================

#guard tokenKinds "0" == some [.integer 0, .newline, .endmarker]
#guard tokenKinds "42" == some [.integer 42, .newline, .endmarker]
#guard tokenKinds "1_000_000" == some [.integer 1000000, .newline, .endmarker]
#guard tokenKinds "0xff" == some [.integer 255, .newline, .endmarker]
#guard tokenKinds "0XFF" == some [.integer 255, .newline, .endmarker]
#guard tokenKinds "0o77" == some [.integer 63, .newline, .endmarker]
#guard tokenKinds "0b1010" == some [.integer 10, .newline, .endmarker]
#guard tokenKinds "0x_ff" == some [.integer 255, .newline, .endmarker]

-- ============================================================
-- Floats
-- ============================================================

#guard tokenKinds "3.14" == some [.float_ 3.14, .newline, .endmarker]
#guard tokenKinds "1e10" == some [.float_ 1e10, .newline, .endmarker]
#guard tokenKinds "1.5e2" == some [.float_ 150.0, .newline, .endmarker]
#guard tokenKinds "0.5" == some [.float_ 0.5, .newline, .endmarker]

-- ============================================================
-- Imaginary
-- ============================================================

#guard tokenKinds "3j" == some [.imaginary 3.0, .newline, .endmarker]
#guard tokenKinds "1.5j" == some [.imaginary 1.5, .newline, .endmarker]

-- ============================================================
-- Strings
-- ============================================================

-- Single-quoted
#guard tokenKinds "'hello'" == some [.string "hello", .newline, .endmarker]
-- Double-quoted
#guard tokenKinds "\"hello\"" == some [.string "hello", .newline, .endmarker]
-- Escape sequences
#guard tokenKinds "'he\\nllo'" == some [.string "he\nllo", .newline, .endmarker]
#guard tokenKinds "'\\t'" == some [.string "\t", .newline, .endmarker]
#guard tokenKinds "'\\\\'" == some [.string "\\", .newline, .endmarker]
-- Raw strings
#guard tokenKinds "r'\\n'" == some [.string "\\n", .newline, .endmarker]
-- Bytes
#guard tokenKinds "b'hi'" == some [.bytes ⟨#[104, 105]⟩, .newline, .endmarker]
-- Triple-quoted
#guard tokenKinds "'''abc'''" == some [.string "abc", .newline, .endmarker]
#guard tokenKinds "\"\"\"abc\"\"\"" == some [.string "abc", .newline, .endmarker]
-- Empty string
#guard tokenKinds "''" == some [.string "", .newline, .endmarker]

-- ============================================================
-- Comments
-- ============================================================

-- Comment-only lines are skipped (no comment tokens emitted)
#guard tokenKinds "# hello" == some [.endmarker]
-- Inline comments on code lines are still emitted
#guard tokenKinds "x # comment" == some [.name "x", .comment "# comment", .newline, .endmarker]
#guard tokenKinds "x # comment\n" == some [.name "x", .comment "# comment", .newline, .endmarker]

-- ============================================================
-- Newlines
-- ============================================================

#guard tokenKinds "x\ny" == some [.name "x", .newline, .name "y", .newline, .endmarker]

-- Inside brackets, newlines become nlToken (non-logical)
#guard tokenKinds "(\n)" == some [.delimiter .lpar, .nlToken, .delimiter .rpar, .newline, .endmarker]

-- ============================================================
-- INDENT / DEDENT
-- ============================================================

-- Simple indent
#guard tokenKinds "if x:\n    y\n" == some [
  .keyword .if_, .name "x", .delimiter .colon, .newline,
  .indent, .name "y", .newline,
  .dedent, .endmarker]

-- Nested indent
#guard tokenKinds "if x:\n    if y:\n        z\n" == some [
  .keyword .if_, .name "x", .delimiter .colon, .newline,
  .indent, .keyword .if_, .name "y", .delimiter .colon, .newline,
  .indent, .name "z", .newline,
  .dedent, .dedent, .endmarker]

-- Dedent to outer level
#guard tokenKinds "if x:\n    y\nz\n" == some [
  .keyword .if_, .name "x", .delimiter .colon, .newline,
  .indent, .name "y", .newline,
  .dedent, .name "z", .newline,
  .endmarker]

-- No indent inside brackets
#guard tokenKinds "x = [\n    1,\n    2\n]\n" == some [
  .name "x", .operator .equal, .delimiter .lsqb, .nlToken,
  .integer 1, .delimiter .comma, .nlToken,
  .integer 2, .nlToken,
  .delimiter .rsqb, .newline,
  .endmarker]

-- ============================================================
-- Composite expressions
-- ============================================================

#guard tokenKinds "x + 1" == some [.name "x", .operator .plus, .integer 1, .newline, .endmarker]
#guard tokenKinds "x = 42" == some [.name "x", .operator .equal, .integer 42, .newline, .endmarker]
#guard tokenKinds "def f(x):" == some [
  .keyword .def_, .name "f", .delimiter .lpar, .name "x", .delimiter .rpar,
  .delimiter .colon, .newline, .endmarker]

-- ============================================================
-- Integration test: a real Python function
-- ============================================================

#guard tokenKinds "def fib(n):\n    if n <= 1:\n        return n\n    return fib(n - 1) + fib(n - 2)\n" == some [
  .keyword .def_, .name "fib", .delimiter .lpar, .name "n", .delimiter .rpar,
    .delimiter .colon, .newline,
  .indent,
    .keyword .if_, .name "n", .operator .lessEqual, .integer 1, .delimiter .colon, .newline,
    .indent,
      .keyword .return_, .name "n", .newline,
    .dedent,
    .keyword .return_, .name "fib", .delimiter .lpar, .name "n", .operator .minus, .integer 1,
      .delimiter .rpar, .operator .plus, .name "fib", .delimiter .lpar, .name "n", .operator .minus,
      .integer 2, .delimiter .rpar, .newline,
  .dedent,
  .endmarker]

-- Integration test: class definition
#guard tokenKinds "class Foo:\n    x = 42\n" == some [
  .keyword .class_, .name "Foo", .delimiter .colon, .newline,
  .indent, .name "x", .operator .equal, .integer 42, .newline,
  .dedent, .endmarker]

-- Integration test: string with assignment
#guard tokenKinds "name = 'hello'\n" == some [
  .name "name", .operator .equal, .string "hello", .newline, .endmarker]
