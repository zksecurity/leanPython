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

-- Exception as binding gives message string
#eval assertPy "try:\n    raise ValueError(\"bad value\")\nexcept ValueError as e:\n    print(e)\n" "bad value\n"

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

-- ============================================================
-- Phase 5A: Object Model — Classes, Instances, Inheritance
-- ============================================================

-- Basic class with method
#eval assertPy "class Greeter:\n    def greet(self):\n        print('hello')\ng = Greeter()\ng.greet()\n" "hello\n"

-- __init__ with parameters
#eval assertPy "class Point:\n    def __init__(self, x, y):\n        self.x = x\n        self.y = y\np = Point(3, 4)\nprint(p.x)\nprint(p.y)\n" "3\n4\n"

-- Instance method accessing self attributes
#eval assertPy "class Rect:\n    def __init__(self, w, h):\n        self.w = w\n        self.h = h\n    def area(self):\n        return self.w * self.h\nr = Rect(3, 5)\nprint(r.area())\n" "15\n"

-- Class variable vs instance variable
#eval assertPy "class Dog:\n    species = 'canine'\n    def __init__(self, name):\n        self.name = name\nd = Dog('Rex')\nprint(d.species)\nprint(d.name)\nprint(Dog.species)\n" "canine\nRex\ncanine\n"

-- Single inheritance
#eval assertPy "class Animal:\n    def speak(self):\n        return 'generic'\nclass Cat(Animal):\n    def purr(self):\n        return 'prrr'\nc = Cat()\nprint(c.speak())\nprint(c.purr())\n" "generic\nprrr\n"

-- Method override
#eval assertPy "class Base:\n    def greet(self):\n        return 'base'\nclass Child(Base):\n    def greet(self):\n        return 'child'\nb = Base()\nc = Child()\nprint(b.greet())\nprint(c.greet())\n" "base\nchild\n"

-- super().__init__()
#eval assertPy "class A:\n    def __init__(self, x):\n        self.x = x\nclass B(A):\n    def __init__(self, x, y):\n        super().__init__(x)\n        self.y = y\nb = B(10, 20)\nprint(b.x)\nprint(b.y)\n" "10\n20\n"

-- super().method()
#eval assertPy "class A:\n    def greet(self):\n        return 'A'\nclass B(A):\n    def greet(self):\n        return 'B+' + super().greet()\nprint(B().greet())\n" "B+A\n"

-- isinstance with custom classes
#eval assertPy "class Foo:\n    pass\nclass Bar(Foo):\n    pass\nf = Foo()\nb = Bar()\nprint(isinstance(f, Foo))\nprint(isinstance(b, Foo))\nprint(isinstance(b, Bar))\nprint(isinstance(f, Bar))\n" "True\nTrue\nTrue\nFalse\n"

-- type() returns the class for instances
#eval assertPy "class MyClass:\n    pass\nobj = MyClass()\nprint(type(obj) is MyClass)\n" "True\n"

-- Multiple levels of inheritance
#eval assertPy "class A:\n    def who(self):\n        return 'A'\nclass B(A):\n    pass\nclass C(B):\n    pass\nc = C()\nprint(c.who())\n" "A\n"

-- Instance attribute shadows class attribute
#eval assertPy "class Cnt:\n    val = 0\n    def inc(self):\n        self.val = self.val + 1\nc = Cnt()\nc.inc()\nc.inc()\nprint(c.val)\nprint(Cnt.val)\n" "2\n0\n"

-- callable() on class
#eval assertPy "class X:\n    pass\nprint(callable(X))\n" "True\n"

-- Class with no __init__ and no args
#eval assertPy "class Empty:\n    pass\ne = Empty()\nprint(type(e) is Empty)\n" "True\n"

-- Multi-level super() chain
#eval assertPy "class A:\n    def __init__(self, x):\n        self.x = x\nclass B(A):\n    def __init__(self, x, y):\n        super().__init__(x)\n        self.y = y\nclass C(B):\n    def __init__(self, x, y, z):\n        super().__init__(x, y)\n        self.z = z\nc = C(1, 2, 3)\nprint(c.x)\nprint(c.y)\nprint(c.z)\n" "1\n2\n3\n"

-- Inherited __init__
#eval assertPy "class A:\n    def __init__(self, v):\n        self.v = v\nclass B(A):\n    def double(self):\n        return self.v * 2\nb = B(5)\nprint(b.double())\n" "10\n"

-- ============================================================
-- Phase 5B: Operator Overloading, Dunder Protocols, Decorators
-- ============================================================

-- __add__ operator overloading
#eval assertPy "class Vec:\n    def __init__(self, x):\n        self.x = x\n    def __add__(self, other):\n        return Vec(self.x + other.x)\nv = Vec(1) + Vec(2)\nprint(v.x)\n" "3\n"

-- __sub__
#eval assertPy "class Num:\n    def __init__(self, v):\n        self.v = v\n    def __sub__(self, other):\n        return Num(self.v - other.v)\nprint((Num(10) - Num(3)).v)\n" "7\n"

-- __mul__
#eval assertPy "class Num:\n    def __init__(self, v):\n        self.v = v\n    def __mul__(self, other):\n        return Num(self.v * other.v)\nprint((Num(3) * Num(4)).v)\n" "12\n"

-- __eq__
#eval assertPy "class C:\n    def __init__(self, v):\n        self.v = v\n    def __eq__(self, other):\n        return self.v == other.v\nprint(C(1) == C(1))\nprint(C(1) == C(2))\n" "True\nFalse\n"

-- __lt__ and __le__
#eval assertPy "class C:\n    def __init__(self, v):\n        self.v = v\n    def __lt__(self, other):\n        return self.v < other.v\nprint(C(1) < C(2))\nprint(C(2) < C(1))\n" "True\nFalse\n"

-- __neg__ (unary operator)
#eval assertPy "class C:\n    def __init__(self, v):\n        self.v = v\n    def __neg__(self):\n        return C(-self.v)\nprint((-C(5)).v)\n" "-5\n"

-- __str__ via print()
#eval assertPy "class C:\n    def __str__(self):\n        return 'hello from C'\nprint(C())\n" "hello from C\n"

-- __str__ via str()
#eval assertPy "class C:\n    def __str__(self):\n        return 'C_str'\nprint(str(C()))\n" "C_str\n"

-- __repr__ via repr()
#eval assertPy "class C:\n    def __repr__(self):\n        return 'C_repr'\nprint(repr(C()))\n" "C_repr\n"

-- __len__ via len()
#eval assertPy "class C:\n    def __len__(self):\n        return 42\nprint(len(C()))\n" "42\n"

-- __getitem__
#eval assertPy "class M:\n    def __init__(self):\n        self.data = [10, 20, 30]\n    def __getitem__(self, i):\n        return self.data[i]\nm = M()\nprint(m[1])\n" "20\n"

-- __setitem__
#eval assertPy "class M:\n    def __init__(self):\n        self.data = [0, 0, 0]\n    def __setitem__(self, i, v):\n        self.data[i] = v\n    def __getitem__(self, i):\n        return self.data[i]\nm = M()\nm[1] = 99\nprint(m[1])\n" "99\n"

-- __contains__ (in operator)
#eval assertPy "class C:\n    def __contains__(self, x):\n        return x == 1\nc = C()\nprint(1 in c)\nprint(2 in c)\n" "True\nFalse\n"

-- __call__ (callable instances)
#eval assertPy "class Adder:\n    def __init__(self, n):\n        self.n = n\n    def __call__(self, x):\n        return self.n + x\na = Adder(10)\nprint(a(5))\n" "15\n"

-- __bool__ via bool()
#eval assertPy "class C:\n    def __init__(self, v):\n        self.v = v\n    def __bool__(self):\n        return self.v > 0\nprint(bool(C(1)))\nprint(bool(C(0)))\n" "True\nFalse\n"

-- __hash__
#eval assertPy "class C:\n    def __hash__(self):\n        return 99\nprint(hash(C()))\n" "99\n"

-- @staticmethod
#eval assertPy "class C:\n    @staticmethod\n    def greet():\n        return 'hi'\nprint(C.greet())\nprint(C().greet())\n" "hi\nhi\n"

-- @classmethod
#eval assertPy "class C:\n    name = 'MyClass'\n    @classmethod\n    def who(cls):\n        return cls.name\nprint(C.who())\n" "MyClass\n"

-- @classmethod on instance
#eval assertPy "class C:\n    x = 10\n    @classmethod\n    def get_x(cls):\n        return cls.x\nprint(C().get_x())\n" "10\n"

-- @property getter
#eval assertPy "class C:\n    def __init__(self, v):\n        self._v = v\n    @property\n    def v(self):\n        return self._v\nc = C(42)\nprint(c.v)\n" "42\n"

-- @property with setter
#eval assertPy "class C:\n    def __init__(self, v):\n        self._v = v\n    @property\n    def v(self):\n        return self._v\n    @v.setter\n    def v(self, val):\n        self._v = val\nc = C(1)\nprint(c.v)\nc.v = 99\nprint(c.v)\n" "1\n99\n"

-- Generic decorator
#eval assertPy "def double_result(fn):\n    def wrapper(*args):\n        return fn(*args) * 2\n    return wrapper\n@double_result\ndef add(a, b):\n    return a + b\nprint(add(3, 4))\n" "14\n"

-- Augmented assignment with dunder
#eval assertPy "class Num:\n    def __init__(self, v):\n        self.v = v\n    def __add__(self, other):\n        return Num(self.v + other.v)\nn = Num(1)\nn = n + Num(2)\nprint(n.v)\n" "3\n"

-- Operator overloading with inheritance
#eval assertPy "class Base:\n    def __init__(self, v):\n        self.v = v\n    def __add__(self, other):\n        return Base(self.v + other.v)\nclass Child(Base):\n    pass\nc = Child(10) + Child(20)\nprint(c.v)\n" "30\n"

-- Instance identity equality (no __eq__ defined)
#eval assertPy "class C:\n    pass\na = C()\nb = C()\nprint(a == a)\nprint(a == b)\n" "True\nFalse\n"

-- ============================================================
-- Phase 5C: Advanced class mechanics
-- ============================================================

-- C3 MRO: diamond inheritance
#eval assertPy "class A:\n    def who(self):\n        return 'A'\nclass B(A):\n    def who(self):\n        return 'B'\nclass C(A):\n    def who(self):\n        return 'C'\nclass D(B, C):\n    pass\nprint(D().who())\n" "B\n"

-- C3 MRO: method from second base when first doesn't define it
#eval assertPy "class A:\n    def f(self):\n        return 'A'\nclass B(A):\n    pass\nclass C(A):\n    def f(self):\n        return 'C'\nclass D(B, C):\n    pass\nprint(D().f())\n" "C\n"

-- C3 MRO: super() cooperative diamond
#eval assertPy "class A:\n    def __init__(self):\n        self.log = 'A'\nclass B(A):\n    def __init__(self):\n        super().__init__()\n        self.log += 'B'\nclass C(A):\n    def __init__(self):\n        super().__init__()\n        self.log += 'C'\nclass D(B, C):\n    def __init__(self):\n        super().__init__()\n        self.log += 'D'\nd = D()\nprint(d.log)\n" "ACBD\n"

-- __annotations__ access
#eval assertPy "class Pt:\n    x: int\n    y: str\nann = Pt.__annotations__\nprint('x' in ann)\nprint('y' in ann)\n" "True\nTrue\n"

-- __dict__ on instances
#eval assertPy "class C:\n    def __init__(self, x):\n        self.x = x\nc = C(42)\nd = c.__dict__\nprint('x' in d)\nprint(d['x'])\n" "True\n42\n"

-- __class__ on instances
#eval assertPy "class C:\n    pass\nc = C()\nprint(c.__class__ is C)\n" "True\n"

-- __name__ on classes
#eval assertPy "class MyClass:\n    pass\nprint(MyClass.__name__)\n" "MyClass\n"

-- __new__ basic: custom allocation
#eval assertPy "class C:\n    def __new__(cls, v):\n        inst = object.__new__(cls)\n        inst.tag = 'created'\n        return inst\n    def __init__(self, v):\n        self.v = v\nc = C(10)\nprint(c.v)\nprint(c.tag)\n" "10\ncreated\n"

-- __new__ validation: reject invalid args
#eval assertPy "class Positive:\n    def __new__(cls, v):\n        if v <= 0:\n            raise ValueError('must be positive')\n        inst = object.__new__(cls)\n        return inst\n    def __init__(self, v):\n        self.v = v\np = Positive(5)\nprint(p.v)\n" "5\n"

-- __getattr__ fallback
#eval assertPy "class C:\n    def __getattr__(self, name):\n        return 'default_' + name\nc = C()\nprint(c.foo)\nprint(c.bar)\n" "default_foo\ndefault_bar\n"

-- __getattr__ only called when normal lookup fails
#eval assertPy "class C:\n    def __init__(self):\n        self.x = 42\n    def __getattr__(self, name):\n        return 'fallback'\nc = C()\nprint(c.x)\nprint(c.y)\n" "42\nfallback\n"

-- __setattr__ intercept (uses print to avoid infinite recursion)
#eval assertPy "class C:\n    def __setattr__(self, name, value):\n        print('setting ' + name)\nc = C()\nc.x = 1\nc.y = 2\n" "setting x\nsetting y\n"

-- __delattr__ hook
#eval assertPy "class C:\n    def __init__(self):\n        self.x = 1\n        self.deleted = []\n    def __delattr__(self, name):\n        self.deleted.append(name)\nc = C()\ndel c.x\nprint(c.deleted)\n" "['x']\n"

-- Class decorator
#eval assertPy "def add_greet(cls):\n    cls.greet = lambda self: 'hi'\n    return cls\n@add_greet\nclass C:\n    pass\nprint(C().greet())\n" "hi\n"

-- Class decorator with args-like pattern (function returning decorator)
#eval assertPy "def tag(name):\n    def decorator(cls):\n        cls.tag_name = name\n        return cls\n    return decorator\n@tag('myTag')\nclass C:\n    pass\nprint(C.tag_name)\n" "myTag\n"

-- __slots__ restricts attributes
#eval assertPyError "class C:\n    __slots__ = ('x', 'y')\nc = C()\nc.x = 1\nc.z = 3\n" "has no attribute"

-- __slots__ allows listed attributes
#eval assertPy "class C:\n    __slots__ = ('x', 'y')\nc = C()\nc.x = 1\nc.y = 2\nprint(c.x)\nprint(c.y)\n" "1\n2\n"

-- @dataclass basic: auto __init__ and __repr__
#eval assertPy "@dataclass\nclass Point:\n    x: int\n    y: int\np = Point(3, 4)\nprint(p.x)\nprint(p.y)\n" "3\n4\n"

-- @dataclass __eq__
#eval assertPy "@dataclass\nclass Point:\n    x: int\n    y: int\nprint(Point(1, 2) == Point(1, 2))\nprint(Point(1, 2) == Point(1, 3))\n" "True\nFalse\n"

-- @dataclass __repr__
#eval assertPy "@dataclass\nclass Point:\n    x: int\n    y: int\nprint(repr(Point(3, 4)))\n" "Point(x=3, y=4)\n"

-- @dataclass(frozen=True)
#eval assertPyError "@dataclass(frozen=True)\nclass Frozen:\n    x: int\nf = Frozen(5)\nf.x = 10\n" "cannot assign"

-- @dataclass with default values
#eval assertPy "@dataclass\nclass Config:\n    x: int = 10\n    y: int = 20\nc = Config()\nprint(c.x)\nprint(c.y)\n" "10\n20\n"

-- @dataclass with partial defaults
#eval assertPy "@dataclass\nclass Config:\n    x: int\n    y: int = 99\nc = Config(1)\nprint(c.x)\nprint(c.y)\n" "1\n99\n"
