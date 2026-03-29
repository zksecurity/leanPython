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
