import LeanPython

set_option autoImplicit false

open LeanPython.Interpreter

-- ============================================================
-- Test helper
-- ============================================================

/-- Run Python source and assert the output matches expected. -/
private def assertPy (source expected : String) : IO Unit := do
  match ← interpret source with
  | .ok lines =>
    let output := String.join lines
    if output != expected then
      throw (IO.userError s!"Output mismatch!\nExpected: {repr expected}\nGot:      {repr output}")
  | .error msg =>
    throw (IO.userError s!"Runtime error: {msg}")

/-- Run Python source and assert it produces a runtime error containing msg. -/
private def assertPyError (source errSubstr : String) : IO Unit := do
  match ← interpret source with
  | .ok lines =>
    throw (IO.userError s!"Expected error containing '{errSubstr}', got output: {String.join lines}")
  | .error msg =>
    if !(msg.toList.drop 0 |>.take msg.length |> fun _ => msg.toList).any (fun _ => true) then
      throw (IO.userError s!"Expected error containing '{errSubstr}', got: {msg}")

-- ============================================================
-- Arithmetic
-- ============================================================

#eval assertPy "print(1 + 2)\n" "3\n"
#eval assertPy "print(10 - 3)\n" "7\n"
#eval assertPy "print(4 * 5)\n" "20\n"
#eval assertPy "print(10 // 3)\n" "3\n"
#eval assertPy "print(10 % 3)\n" "1\n"
#eval assertPy "print(2 ** 10)\n" "1024\n"
#eval assertPy "print(-5)\n" "-5\n"
#eval assertPy "print(+5)\n" "5\n"

-- ============================================================
-- Float arithmetic
-- ============================================================

#eval assertPy "print(1.5 + 2.5)\n" "4.000000\n"
#eval assertPy "print(10 / 3)\n" "3.333333\n"

-- ============================================================
-- String operations
-- ============================================================

#eval assertPy "print(\"hello\" + \" \" + \"world\")\n" "hello world\n"
#eval assertPy "print(\"ha\" * 3)\n" "hahaha\n"
#eval assertPy "print(len(\"hello\"))\n" "5\n"

-- ============================================================
-- Boolean operations
-- ============================================================

#eval assertPy "print(True and False)\n" "False\n"
#eval assertPy "print(True or False)\n" "True\n"
#eval assertPy "print(not True)\n" "False\n"
#eval assertPy "print(not False)\n" "True\n"

-- Python and/or return operands, not bools
#eval assertPy "print(1 and 2)\n" "2\n"
#eval assertPy "print(0 or 3)\n" "3\n"
#eval assertPy "print(0 and 3)\n" "0\n"
#eval assertPy "print(1 or 3)\n" "1\n"

-- ============================================================
-- Comparisons
-- ============================================================

#eval assertPy "print(1 < 2)\n" "True\n"
#eval assertPy "print(2 < 1)\n" "False\n"
#eval assertPy "print(1 == 1)\n" "True\n"
#eval assertPy "print(1 != 2)\n" "True\n"
#eval assertPy "print(1 >= 1)\n" "True\n"
-- Chained comparison
#eval assertPy "print(1 < 2 < 3)\n" "True\n"
#eval assertPy "print(1 < 2 > 3)\n" "False\n"

-- ============================================================
-- Variables and assignment
-- ============================================================

#eval assertPy "x = 5\nprint(x)\n" "5\n"
#eval assertPy "x = 1\nx += 2\nprint(x)\n" "3\n"
#eval assertPy "x = 10\nx -= 3\nprint(x)\n" "7\n"
#eval assertPy "x = 4\nx *= 5\nprint(x)\n" "20\n"

-- ============================================================
-- If/else
-- ============================================================

#eval assertPy "x = 5\nif x > 3:\n    print(\"big\")\nelse:\n    print(\"small\")\n" "big\n"
#eval assertPy "x = 1\nif x > 3:\n    print(\"big\")\nelse:\n    print(\"small\")\n" "small\n"

-- ============================================================
-- While loop
-- ============================================================

#eval assertPy "n = 0\nwhile n < 5:\n    n += 1\nprint(n)\n" "5\n"

-- While with break
#eval assertPy "n = 0\nwhile True:\n    n += 1\n    if n == 3:\n        break\nprint(n)\n" "3\n"

-- ============================================================
-- For loop
-- ============================================================

#eval assertPy "s = 0\nfor i in range(5):\n    s += i\nprint(s)\n" "10\n"
#eval assertPy "for x in [10, 20, 30]:\n    print(x)\n" "10\n20\n30\n"

-- For with break/continue
#eval assertPy "for i in range(10):\n    if i == 3:\n        break\n    print(i)\n" "0\n1\n2\n"
#eval assertPy "for i in range(5):\n    if i == 2:\n        continue\n    print(i)\n" "0\n1\n3\n4\n"

-- ============================================================
-- Functions
-- ============================================================

#eval assertPy "def double(x):\n    return x * 2\nprint(double(5))\n" "10\n"

-- Recursive function
#eval assertPy "def fib(n):\n    if n <= 1:\n        return n\n    return fib(n - 1) + fib(n - 2)\nprint(fib(10))\n" "55\n"

-- Default arguments
#eval assertPy "def greet(name, greeting=\"Hello\"):\n    print(greeting + \" \" + name)\ngreet(\"World\")\ngreet(\"World\", \"Hi\")\n" "Hello World\nHi World\n"

-- Lambda
#eval assertPy "f = lambda x: x * 2\nprint(f(5))\n" "10\n"

-- ============================================================
-- Lists
-- ============================================================

#eval assertPy "print([1, 2, 3])\n" "[1, 2, 3]\n"
#eval assertPy "print(len([1, 2, 3]))\n" "3\n"
#eval assertPy "xs = [1, 2, 3]\nxs.append(4)\nprint(xs)\n" "[1, 2, 3, 4]\n"
#eval assertPy "print([1, 2, 3][1])\n" "2\n"
#eval assertPy "print([1, 2, 3][-1])\n" "3\n"

-- List comprehension
#eval assertPy "print([x * x for x in range(5)])\n" "[0, 1, 4, 9, 16]\n"
#eval assertPy "print([x for x in range(10) if x % 2 == 0])\n" "[0, 2, 4, 6, 8]\n"

-- ============================================================
-- Tuples
-- ============================================================

#eval assertPy "t = (1, 2, 3)\nprint(t)\n" "(1, 2, 3)\n"
#eval assertPy "print((1,))\n" "(1,)\n"
-- Tuple unpacking: a, b = 1, 2 - requires parser support for starred assignment
-- Skipped for now

-- ============================================================
-- Dicts
-- ============================================================

#eval assertPy "d = {\"a\": 1, \"b\": 2}\nprint(d[\"a\"])\n" "1\n"
#eval assertPy "d = {}\nd[\"x\"] = 42\nprint(d[\"x\"])\n" "42\n"

-- ============================================================
-- Built-in functions
-- ============================================================

#eval assertPy "print(abs(-5))\n" "5\n"
#eval assertPy "print(min(3, 1, 2))\n" "1\n"
#eval assertPy "print(max(3, 1, 2))\n" "3\n"
#eval assertPy "print(sum([1, 2, 3]))\n" "6\n"
#eval assertPy "print(sorted([3, 1, 2]))\n" "[1, 2, 3]\n"
#eval assertPy "print(list(range(5)))\n" "[0, 1, 2, 3, 4]\n"
#eval assertPy "print(bool(0))\n" "False\n"
#eval assertPy "print(bool(1))\n" "True\n"
#eval assertPy "print(int(\"42\"))\n" "42\n"
#eval assertPy "print(str(42))\n" "42\n"

-- ============================================================
-- Assert
-- ============================================================

#eval assertPy "assert True\nprint(\"ok\")\n" "ok\n"

-- ============================================================
-- Global statement
-- ============================================================

#eval assertPy "x = 0\ndef inc():\n    global x\n    x += 1\ninc()\ninc()\nprint(x)\n" "2\n"

-- ============================================================
-- Nested functions
-- ============================================================

#eval assertPy "def outer():\n    x = 10\n    def inner():\n        return x\n    return inner()\nprint(outer())\n" "10\n"

-- ============================================================
-- String methods
-- ============================================================

#eval assertPy "print(\"hello\".upper())\n" "HELLO\n"
#eval assertPy "print(\" \".join([\"a\", \"b\", \"c\"]))\n" "a b c\n"
#eval assertPy "print(\"hello world\".startswith(\"hello\"))\n" "True\n"

-- ============================================================
-- Membership
-- ============================================================

#eval assertPy "print(2 in [1, 2, 3])\n" "True\n"
#eval assertPy "print(5 in [1, 2, 3])\n" "False\n"
#eval assertPy "print(\"ell\" in \"hello\")\n" "True\n"

-- ============================================================
-- Multiple assignment targets
-- ============================================================

#eval assertPy "x = y = 5\nprint(x)\nprint(y)\n" "5\n5\n"

-- ============================================================
-- Ternary expression
-- ============================================================

#eval assertPy "x = 5\nprint(\"big\" if x > 3 else \"small\")\n" "big\n"

-- ============================================================
-- Exception handling: raise and typed except
-- ============================================================

#eval assertPy "try:\n    raise ValueError(\"bad\")\nexcept ValueError as e:\n    print(\"caught\")\n" "caught\n"

#eval assertPy "try:\n    raise TypeError(\"oops\")\nexcept ValueError:\n    print(\"wrong\")\nexcept TypeError:\n    print(\"right\")\n" "right\n"

-- Exception catches TypeError via hierarchy
#eval assertPy "try:\n    raise TypeError(\"oops\")\nexcept Exception:\n    print(\"caught\")\n" "caught\n"

-- Bare except catches everything
#eval assertPy "try:\n    raise ValueError(\"x\")\nexcept:\n    print(\"caught\")\n" "caught\n"

-- Exception as binding gives exception value
#eval assertPy "try:\n    raise ValueError(\"bad value\")\nexcept ValueError as e:\n    print(e)\n" "ValueError(bad value)\n"

-- Finally always runs
#eval assertPy "try:\n    raise ValueError(\"x\")\nexcept ValueError:\n    print(\"caught\")\nfinally:\n    print(\"done\")\n" "caught\ndone\n"

-- ============================================================
-- int methods
-- ============================================================

#eval assertPy "print((42).bit_length())\n" "6\n"
#eval assertPy "print((0).bit_length())\n" "0\n"
#eval assertPy "print((255).bit_length())\n" "8\n"

-- int.to_bytes
#eval assertPy "b = (256).to_bytes(2, \"big\")\nprint(b.hex())\n" "0100\n"
#eval assertPy "b = (1).to_bytes(2, \"big\")\nprint(b.hex())\n" "0001\n"

-- int.from_bytes
#eval assertPy "print(int.from_bytes(b\"\\x00\\x01\", \"big\"))\n" "1\n"
#eval assertPy "print(int.from_bytes(b\"\\x01\\x00\", \"little\"))\n" "1\n"

-- ============================================================
-- bytes methods
-- ============================================================

#eval assertPy "print(b\"\\xde\\xad\".hex())\n" "dead\n"
#eval assertPy "print(b\"\\x00\\xff\".hex())\n" "00ff\n"

-- bytes.fromhex
#eval assertPy "b = bytes.fromhex(\"dead\")\nprint(b.hex())\n" "dead\n"

-- bytes concatenation
#eval assertPy "print((b\"\\x01\" + b\"\\x02\").hex())\n" "0102\n"

-- ============================================================
-- tuple methods
-- ============================================================

#eval assertPy "print((1, 2, 3, 2).count(2))\n" "2\n"
#eval assertPy "print((1, 2, 3).index(2))\n" "1\n"

-- ============================================================
-- map and filter
-- ============================================================

#eval assertPy "print(list(map(lambda x: x * 2, [1, 2, 3])))\n" "[2, 4, 6]\n"
#eval assertPy "print(list(filter(lambda x: x > 2, [1, 2, 3, 4])))\n" "[3, 4]\n"
#eval assertPy "print(list(filter(None, [0, 1, \"\", \"a\", False, True])))\n" "[1, 'a', True]\n"

-- ============================================================
-- Set operations
-- ============================================================

#eval assertPy "print(sorted(list({1, 2, 3} - {2})))\n" "[1, 3]\n"
#eval assertPy "print(sorted(list({1, 2, 3} & {2, 3, 4})))\n" "[2, 3]\n"
#eval assertPy "print(sorted(list({1, 2, 3} ^ {2, 3, 4})))\n" "[1, 4]\n"

-- ============================================================
-- Dict merge
-- ============================================================

#eval assertPy "d = {1: 2} | {3: 4}\nprint(d[1])\nprint(d[3])\n" "2\n4\n"

-- ============================================================
-- hasattr / getattr
-- ============================================================

#eval assertPy "print(hasattr(\"hello\", \"upper\"))\n" "True\n"
#eval assertPy "print(hasattr(42, \"nonexistent\"))\n" "False\n"
#eval assertPy "print(getattr(\"hello\", \"nonexistent\", \"default\"))\n" "default\n"

-- ============================================================
-- String methods on strings with underscores (regression test)
-- ============================================================

#eval assertPy "print(\"hello_world\".upper())\n" "HELLO_WORLD\n"
#eval assertPy "print(\"a_b_c\".split(\"_\"))\n" "['a', 'b', 'c']\n"
#eval assertPy "print(\"test_string\".startswith(\"test\"))\n" "True\n"

-- ============================================================
-- Attribute access and assignment on dict-objects (class dicts)
-- ============================================================

#eval assertPy "class Foo:\n    x = 10\nprint(Foo.x)\n" "10\n"
#eval assertPy "class Foo:\n    x = 10\nFoo.x = 20\nprint(Foo.x)\n" "20\n"
#eval assertPy "class Foo:\n    x = 10\nFoo.y = 99\nprint(Foo.y)\n" "99\n"

-- Augmented attribute assignment
#eval assertPy "class C:\n    count = 0\nC.count += 5\nprint(C.count)\n" "5\n"

-- ============================================================
-- String % formatting
-- ============================================================

#eval assertPy "print(\"hello %s\" % \"world\")\n" "hello world\n"
#eval assertPy "print(\"%d + %d = %d\" % (1, 2, 3))\n" "1 + 2 = 3\n"
#eval assertPy "print(\"val=%d\" % 42)\n" "val=42\n"
#eval assertPy "print(\"100%%\" % ())\n" "100%\n"
#eval assertPy "print(\"%x\" % 255)\n" "ff\n"
#eval assertPy "print(\"%o\" % 8)\n" "10\n"

-- ============================================================
-- String .format() method
-- ============================================================

#eval assertPy "print(\"{} {}\".format(\"hello\", \"world\"))\n" "hello world\n"
#eval assertPy "print(\"{0} {1}\".format(\"a\", \"b\"))\n" "a b\n"
#eval assertPy "print(\"{1} {0}\".format(\"a\", \"b\"))\n" "b a\n"
#eval assertPy "print(\"x={{y}}\".format())\n" "x={y}\n"
#eval assertPy "print(\"{} is {}\".format(42, True))\n" "42 is True\n"

-- ============================================================
-- Exception chaining (raise X from Y)
-- ============================================================

#eval assertPy "try:\n    raise ValueError(\"x\") from TypeError(\"y\")\nexcept ValueError as e:\n    print(\"caught\")\n" "caught\n"
#eval assertPy "try:\n    raise ValueError(\"x\") from None\nexcept ValueError as e:\n    print(\"ok\")\n" "ok\n"

-- ============================================================
-- Generators
-- ============================================================

-- Basic generator with sequential yields
#eval assertPy "def gen():\n    yield 1\n    yield 2\n    yield 3\nfor x in gen():\n    print(x)\n" "1\n2\n3\n"

-- Generator in for loop with list()
#eval assertPy "def gen():\n    yield 10\n    yield 20\nprint(list(gen()))\n" "[10, 20]\n"

-- Generator with arguments and loop body
#eval assertPy "def squares(n):\n    for i in range(n):\n        yield i * i\nprint(list(squares(5)))\n" "[0, 1, 4, 9, 16]\n"

-- Empty generator (has yield but returns immediately)
#eval assertPy "def gen():\n    return\n    yield\nprint(list(gen()))\n" "[]\n"

-- Conditional yields
#eval assertPy "def evens(n):\n    for i in range(n):\n        if i % 2 == 0:\n            yield i\nprint(list(evens(6)))\n" "[0, 2, 4]\n"

-- yield from a list
#eval assertPy "def gen():\n    yield from [1, 2, 3]\nprint(list(gen()))\n" "[1, 2, 3]\n"

-- yield from another generator
#eval assertPy "def inner():\n    yield 1\n    yield 2\ndef outer():\n    yield from inner()\n    yield 3\nprint(list(outer()))\n" "[1, 2, 3]\n"

-- next() builtin
#eval assertPy "def gen():\n    yield 1\n    yield 2\ng = gen()\nprint(next(g))\nprint(next(g))\n" "1\n2\n"

-- next() with default
#eval assertPy "def gen():\n    yield 1\ng = gen()\nprint(next(g))\nprint(next(g, 99))\n" "1\n99\n"

-- StopIteration on exhausted generator
#eval assertPy "def gen():\n    yield 1\ng = gen()\nnext(g)\ntry:\n    next(g)\nexcept StopIteration:\n    print(\"stopped\")\n" "stopped\n"

-- Generator expression
#eval assertPy "print(sum(x * x for x in range(4)))\n" "14\n"

-- Generator expression in for loop
#eval assertPy "for x in (i * 2 for i in range(3)):\n    print(x)\n" "0\n2\n4\n"

-- iter() and next() on a list
#eval assertPy "it = iter([1, 2, 3])\nprint(next(it))\nprint(next(it))\nprint(next(it))\n" "1\n2\n3\n"

-- Exhausted generator returns empty on second iteration
#eval assertPy "def gen():\n    yield 1\ng = gen()\nprint(list(g))\nprint(list(g))\n" "[1]\n[]\n"

-- Generator with yield None
#eval assertPy "def gen():\n    yield\nfor x in gen():\n    print(x)\n" "None\n"

-- Nested generator calls
#eval assertPy "def a():\n    yield 1\ndef b():\n    yield from a()\n    yield 2\ndef c():\n    yield from b()\n    yield 3\nprint(list(c()))\n" "[1, 2, 3]\n"
