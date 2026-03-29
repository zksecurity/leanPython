import Lython

set_option autoImplicit false

open Lython.Parser (parse)
open Lython.AST (dumpExpr dumpStmt dumpModule Module)

/-- Parse source and return the AST dump string, or none on error. -/
private def parseModStr (source : String) : Option String :=
  match parse source with
  | .ok m => some (dumpModule m)
  | .error _ => none

/-- Parse source as a single expression statement and dump the expression. -/
private def parseExprStr (source : String) : Option String :=
  match parse source with
  | .ok (.module [.expr e _]) => some (dumpExpr e)
  | _ => none

/-- Parse source and dump all statements. -/
private def parseStmtsStr (source : String) : Option String :=
  match parse source with
  | .ok (.module stmts) =>
    some ("[" ++ ", ".intercalate (stmts.map dumpStmt) ++ "]")
  | .error _ => none

-- ============================================================
-- Expression tests: atoms
-- ============================================================

#guard parseExprStr "42\n" == some "Int(42)"
#guard parseExprStr "3.14\n" == some "Float(3.140000)"
#guard parseExprStr "True\n" == some "True"
#guard parseExprStr "False\n" == some "False"
#guard parseExprStr "None\n" == some "None"
#guard parseExprStr "x\n" == some "Name(x)"
#guard parseExprStr "'hello'\n" == some "Str(hello)"
#guard parseExprStr "...\n" == some "Ellipsis"

-- ============================================================
-- Expression tests: binary operators and precedence
-- ============================================================

#guard parseExprStr "1 + 2\n" == some "BinOp(Int(1), Add, Int(2))"
#guard parseExprStr "1 - 2\n" == some "BinOp(Int(1), Sub, Int(2))"
#guard parseExprStr "2 * 3\n" == some "BinOp(Int(2), Mult, Int(3))"
#guard parseExprStr "6 / 2\n" == some "BinOp(Int(6), Div, Int(2))"
#guard parseExprStr "7 // 2\n" == some "BinOp(Int(7), FloorDiv, Int(2))"
#guard parseExprStr "7 % 3\n" == some "BinOp(Int(7), Mod, Int(3))"
#guard parseExprStr "2 ** 3\n" == some "BinOp(Int(2), Pow, Int(3))"

-- Precedence: * binds tighter than +
#guard parseExprStr "1 + 2 * 3\n" == some "BinOp(Int(1), Add, BinOp(Int(2), Mult, Int(3)))"

-- Precedence: ** binds tighter than unary -
#guard parseExprStr "-2 ** 3\n" == some "UnaryOp(USub, BinOp(Int(2), Pow, Int(3)))"

-- Left associativity: 1 - 2 - 3 = (1 - 2) - 3
#guard parseExprStr "1 - 2 - 3\n" == some "BinOp(BinOp(Int(1), Sub, Int(2)), Sub, Int(3))"

-- ============================================================
-- Expression tests: unary operators
-- ============================================================

#guard parseExprStr "-x\n" == some "UnaryOp(USub, Name(x))"
#guard parseExprStr "+x\n" == some "UnaryOp(UAdd, Name(x))"
#guard parseExprStr "~x\n" == some "UnaryOp(Invert, Name(x))"
#guard parseExprStr "not x\n" == some "UnaryOp(Not, Name(x))"

-- ============================================================
-- Expression tests: comparison (chained)
-- ============================================================

#guard parseExprStr "a < b\n" == some "Compare(Name(a), [(Lt, Name(b))])"
#guard parseExprStr "a < b <= c\n" == some "Compare(Name(a), [(Lt, Name(b)), (LtE, Name(c))])"
#guard parseExprStr "a == b\n" == some "Compare(Name(a), [(Eq, Name(b))])"
#guard parseExprStr "a != b\n" == some "Compare(Name(a), [(NotEq, Name(b))])"
#guard parseExprStr "a is b\n" == some "Compare(Name(a), [(Is, Name(b))])"
#guard parseExprStr "a is not b\n" == some "Compare(Name(a), [(IsNot, Name(b))])"
#guard parseExprStr "a in b\n" == some "Compare(Name(a), [(In, Name(b))])"
#guard parseExprStr "a not in b\n" == some "Compare(Name(a), [(NotIn, Name(b))])"

-- ============================================================
-- Expression tests: boolean operators
-- ============================================================

#guard parseExprStr "a and b\n" == some "BoolOp(And, [Name(a), Name(b)])"
#guard parseExprStr "a or b\n" == some "BoolOp(Or, [Name(a), Name(b)])"
#guard parseExprStr "a or b or c\n" == some "BoolOp(Or, [Name(a), Name(b), Name(c)])"

-- ============================================================
-- Expression tests: bitwise operators
-- ============================================================

#guard parseExprStr "a & b\n" == some "BinOp(Name(a), BitAnd, Name(b))"
#guard parseExprStr "a | b\n" == some "BinOp(Name(a), BitOr, Name(b))"
#guard parseExprStr "a ^ b\n" == some "BinOp(Name(a), BitXor, Name(b))"
#guard parseExprStr "a << 2\n" == some "BinOp(Name(a), LShift, Int(2))"
#guard parseExprStr "a >> 2\n" == some "BinOp(Name(a), RShift, Int(2))"

-- ============================================================
-- Expression tests: conditional (ternary if)
-- ============================================================

#guard parseExprStr "a if b else c\n" == some "IfExp(Name(b), Name(a), Name(c))"

-- ============================================================
-- Expression tests: primary (attribute, call, subscript)
-- ============================================================

#guard parseExprStr "a.b\n" == some "Attr(Name(a), b)"
#guard parseExprStr "a.b.c\n" == some "Attr(Attr(Name(a), b), c)"
#guard parseExprStr "f()\n" == some "Call(Name(f), [], [])"
#guard parseExprStr "f(x)\n" == some "Call(Name(f), [Name(x)], [])"
#guard parseExprStr "f(x, y)\n" == some "Call(Name(f), [Name(x), Name(y)], [])"
#guard parseExprStr "f(x=1)\n" == some "Call(Name(f), [], [Kw(x, Int(1))])"
#guard parseExprStr "a[0]\n" == some "Subscript(Name(a), Int(0))"
#guard parseExprStr "a[1:2]\n" == some "Subscript(Name(a), Slice(Int(1), Int(2), None))"
#guard parseExprStr "a[:]\n" == some "Subscript(Name(a), Slice(None, None, None))"
#guard parseExprStr "a[::2]\n" == some "Subscript(Name(a), Slice(None, None, Int(2)))"

-- ============================================================
-- Expression tests: containers
-- ============================================================

#guard parseExprStr "[]\n" == some "List([])"
#guard parseExprStr "[1, 2, 3]\n" == some "List([Int(1), Int(2), Int(3)])"
#guard parseExprStr "()\n" == some "Tuple([])"
#guard parseExprStr "(1, 2)\n" == some "Tuple([Int(1), Int(2)])"
#guard parseExprStr "(1,)\n" == some "Tuple([Int(1)])"
-- Parenthesized expression (not a tuple)
#guard parseExprStr "(1)\n" == some "Int(1)"
#guard parseExprStr "{}\n" == some "Dict([])"
#guard parseExprStr "{1, 2}\n" == some "Set([Int(1), Int(2)])"
#guard parseExprStr "{'a': 1}\n" == some "Dict([(Str(a), Int(1))])"

-- ============================================================
-- Expression tests: starred
-- ============================================================

#guard parseExprStr "*x\n" == some "Starred(Name(x))"

-- ============================================================
-- Expression tests: comprehensions
-- ============================================================

#guard parseExprStr "[x for x in xs]\n" == some "ListComp(Name(x), [Comp(Name(x), Name(xs), [], false)])"
#guard parseExprStr "{x for x in xs}\n" == some "SetComp(Name(x), [Comp(Name(x), Name(xs), [], false)])"
#guard parseExprStr "{k: v for k, v in items}\n" ==
  some "DictComp(Name(k), Name(v), [Comp(Tuple([Name(k), Name(v)]), Name(items), [], false)])"
#guard parseExprStr "[x for x in xs if x > 0]\n" ==
  some "ListComp(Name(x), [Comp(Name(x), Name(xs), [Compare(Name(x), [(Gt, Int(0))])], false)])"

-- ============================================================
-- Statement tests: simple statements
-- ============================================================

#guard parseStmtsStr "pass\n" == some "[Pass]"
#guard parseStmtsStr "break\n" == some "[Break]"
#guard parseStmtsStr "continue\n" == some "[Continue]"
#guard parseStmtsStr "return\n" == some "[Return(None)]"
#guard parseStmtsStr "return 42\n" == some "[Return(Int(42))]"
#guard parseStmtsStr "raise\n" == some "[Raise(None, None)]"
#guard parseStmtsStr "raise ValueError\n" == some "[Raise(Name(ValueError), None)]"

-- ============================================================
-- Statement tests: assignment
-- ============================================================

#guard parseStmtsStr "x = 1\n" == some "[Assign([Name(x)], Int(1))]"
#guard parseStmtsStr "x = y = 1\n" == some "[Assign([Name(x), Name(y)], Int(1))]"
#guard parseStmtsStr "x += 1\n" == some "[AugAssign(Name(x), Add, Int(1))]"
#guard parseStmtsStr "x: int\n" == some "[AnnAssign(Name(x), Name(int), None)]"
#guard parseStmtsStr "x: int = 1\n" == some "[AnnAssign(Name(x), Name(int), Int(1))]"

-- ============================================================
-- Statement tests: assert
-- ============================================================

#guard parseStmtsStr "assert x\n" == some "[Assert(Name(x), None)]"
#guard parseStmtsStr "assert x, 'msg'\n" == some "[Assert(Name(x), Str(msg))]"

-- ============================================================
-- Statement tests: del
-- ============================================================

#guard parseStmtsStr "del x\n" == some "[Delete([Name(x)])]"

-- ============================================================
-- Statement tests: global / nonlocal
-- ============================================================

#guard parseStmtsStr "global x\n" == some "[Global([x])]"
#guard parseStmtsStr "global x, y\n" == some "[Global([x, y])]"
#guard parseStmtsStr "nonlocal x\n" == some "[Nonlocal([x])]"

-- ============================================================
-- Statement tests: import
-- ============================================================

#guard parseStmtsStr "import os\n" == some "[Import([Alias(os, None)])]"
#guard parseStmtsStr "import os.path\n" == some "[Import([Alias(os.path, None)])]"
#guard parseStmtsStr "import os as o\n" == some "[Import([Alias(os, o)])]"
#guard parseStmtsStr "from os import path\n" == some "[ImportFrom(os, [Alias(path, None)])]"
#guard parseStmtsStr "from os import path as p\n" == some "[ImportFrom(os, [Alias(path, p)])]"
#guard parseStmtsStr "from . import utils\n" == some "[ImportFrom(None, [Alias(utils, None)])]"
#guard parseStmtsStr "from .. import utils\n" == some "[ImportFrom(None, [Alias(utils, None)])]"

-- ============================================================
-- Statement tests: if
-- ============================================================

#guard parseStmtsStr "if x:\n    pass\n" == some "[If(Name(x), [Pass], [])]"
#guard parseStmtsStr "if x:\n    pass\nelse:\n    pass\n" ==
  some "[If(Name(x), [Pass], [Pass])]"
#guard parseStmtsStr "if x:\n    pass\nelif y:\n    pass\n" ==
  some "[If(Name(x), [Pass], [If(Name(y), [Pass], [])])]"

-- ============================================================
-- Statement tests: while
-- ============================================================

#guard parseStmtsStr "while x:\n    pass\n" == some "[While(Name(x), [Pass], [])]"

-- ============================================================
-- Statement tests: for
-- ============================================================

#guard parseStmtsStr "for x in xs:\n    pass\n" == some "[For(Name(x), Name(xs), [Pass], [])]"

-- ============================================================
-- Statement tests: function def
-- ============================================================

#guard parseStmtsStr "def f():\n    pass\n" == some "[FunctionDef(f, [Pass])]"
#guard parseStmtsStr "def f():\n    return 1\n" == some "[FunctionDef(f, [Return(Int(1))])]"

-- ============================================================
-- Statement tests: class def
-- ============================================================

#guard parseStmtsStr "class Foo:\n    pass\n" == some "[ClassDef(Foo, [], [Pass])]"
#guard parseStmtsStr "class Foo(Bar):\n    pass\n" == some "[ClassDef(Foo, [Name(Bar)], [Pass])]"

-- ============================================================
-- Statement tests: try/except
-- ============================================================

#guard parseStmtsStr "try:\n    pass\nexcept:\n    pass\n" ==
  some "[Try([Pass], [Handler(None, None, [Pass])], [], [])]"
#guard parseStmtsStr "try:\n    pass\nexcept ValueError as e:\n    pass\n" ==
  some "[Try([Pass], [Handler(Name(ValueError), e, [Pass])], [], [])]"

-- ============================================================
-- Statement tests: with
-- ============================================================

#guard parseStmtsStr "with f() as x:\n    pass\n" ==
  some "[With([WithItem(Call(Name(f), [], []), Name(x))], [Pass])]"

-- ============================================================
-- Statement tests: decorators
-- ============================================================

#guard parseStmtsStr "@deco\ndef f():\n    pass\n" ==
  some "[FunctionDef(f, [Pass])]"

-- ============================================================
-- Integration tests: real Python snippets
-- ============================================================

-- Simple fibonacci
#guard (parseStmtsStr "def fib(n):\n    if n <= 1:\n        return n\n    return fib(n - 1) + fib(n - 2)\n").isSome

-- Class with method
#guard (parseStmtsStr "class Foo:\n    def __init__(self):\n        self.x = 0\n").isSome

-- Multiple statements
#guard parseStmtsStr "x = 1\ny = 2\n" == some "[Assign([Name(x)], Int(1)), Assign([Name(y)], Int(2))]"

-- Semicolons
#guard parseStmtsStr "x = 1; y = 2\n" == some "[Assign([Name(x)], Int(1)), Assign([Name(y)], Int(2))]"
