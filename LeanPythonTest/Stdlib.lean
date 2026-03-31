import LeanPython

set_option autoImplicit false

open LeanPython.Interpreter

-- ============================================================
-- Test helpers (same pattern as Module.lean)
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

/-- Check if needle is a substring of haystack. -/
private def isSubstr (needle haystack : String) : Bool :=
  let nChars := needle.toList
  let hChars := haystack.toList
  let nLen := nChars.length
  let hLen := hChars.length
  if nLen > hLen then false
  else
    (List.range (hLen - nLen + 1)).any fun i =>
      (hChars.drop i |>.take nLen) == nChars

/-- Run Python source and assert it produces a runtime error containing msg. -/
private def assertPyError (source errSubstr : String) : IO Unit := do
  match ← interpret source with
  | .ok lines =>
    throw (IO.userError s!"Expected error containing '{errSubstr}', got output: {String.join lines}")
  | .error msg =>
    if !isSubstr errSubstr msg then
      throw (IO.userError s!"Expected error containing '{errSubstr}', got: {msg}")

-- ============================================================
-- math module tests
-- ============================================================

-- math.ceil
#eval assertPy "import math\nprint(math.ceil(3.2))" "4\n"
#eval assertPy "import math\nprint(math.ceil(-3.2))" "-3\n"
#eval assertPy "import math\nprint(math.ceil(5))" "5\n"
#eval assertPy "import math\nprint(math.ceil(0.0))" "0\n"

-- math.floor
#eval assertPy "import math\nprint(math.floor(3.9))" "3\n"
#eval assertPy "import math\nprint(math.floor(-3.9))" "-4\n"
#eval assertPy "import math\nprint(math.floor(5))" "5\n"

-- math.sqrt
#eval assertPy "import math\nprint(math.sqrt(4.0))" "2.000000\n"
#eval assertPy "import math\nprint(math.sqrt(0.0))" "0.000000\n"
#eval assertPy "import math\nprint(math.sqrt(9))" "3.000000\n"
#eval assertPyError "import math\nmath.sqrt(-1)" "math domain error"

-- math.log2
#eval assertPy "import math\nprint(math.log2(8))" "3.000000\n"
#eval assertPy "import math\nprint(math.log2(1))" "0.000000\n"
#eval assertPyError "import math\nmath.log2(0)" "math domain error"

-- math.log
#eval assertPy "import math\nprint(math.log(1))" "0.000000\n"
#eval assertPy "import math\nprint(math.log(100, 10))" "2.000000\n"
#eval assertPyError "import math\nmath.log(0)" "math domain error"

-- math constants
#eval assertPy "import math\nprint(math.inf > 1000000)" "True\n"
#eval assertPy "import math\nprint(math.pi > 3.14)" "True\n"
#eval assertPy "import math\nprint(math.e > 2.71)" "True\n"

-- math.fabs
#eval assertPy "import math\nprint(math.fabs(-3.5))" "3.500000\n"
#eval assertPy "import math\nprint(math.fabs(3.5))" "3.500000\n"

-- math.isnan / math.isinf
#eval assertPy "import math\nprint(math.isnan(math.nan))" "True\n"
#eval assertPy "import math\nprint(math.isnan(1.0))" "False\n"
#eval assertPy "import math\nprint(math.isinf(math.inf))" "True\n"
#eval assertPy "import math\nprint(math.isinf(1.0))" "False\n"

-- from math import
#eval assertPy "from math import ceil, floor\nprint(ceil(2.3))\nprint(floor(2.9))" "3\n2\n"
#eval assertPy "from math import pi\nprint(pi > 3.14)" "True\n"

-- ============================================================
-- copy module tests
-- ============================================================

#eval assertPy "import copy\na = [1, 2, 3]\nb = copy.copy(a)\nb.append(4)\nprint(a)\nprint(b)" "[1, 2, 3]\n[1, 2, 3, 4]\n"
#eval assertPy "import copy\na = {'x': 1}\nb = copy.copy(a)\nb['y'] = 2\nprint(len(a))\nprint(len(b))" "1\n2\n"
#eval assertPy "import copy\na = 42\nb = copy.copy(a)\nprint(b)" "42\n"

-- deepcopy
#eval assertPy "import copy\na = [[1, 2], [3, 4]]\nb = copy.deepcopy(a)\nb[0].append(99)\nprint(a)\nprint(b)" "[[1, 2], [3, 4]]\n[[1, 2, 99], [3, 4]]\n"

-- ============================================================
-- operator module tests
-- ============================================================

#eval assertPy "import operator\nprint(operator.add(3, 4))" "7\n"
#eval assertPy "import operator\nprint(operator.sub(10, 3))" "7\n"
#eval assertPy "import operator\nprint(operator.mul(3, 4))" "12\n"
#eval assertPy "import operator\nprint(operator.eq(1, 1))" "True\n"
#eval assertPy "import operator\nprint(operator.ne(1, 2))" "True\n"
#eval assertPy "import operator\nprint(operator.lt(1, 2))" "True\n"
#eval assertPy "import operator\nprint(operator.le(2, 2))" "True\n"
#eval assertPy "import operator\nprint(operator.gt(3, 2))" "True\n"
#eval assertPy "import operator\nprint(operator.ge(2, 2))" "True\n"
#eval assertPy "import operator\nprint(operator.neg(5))" "-5\n"
#eval assertPy "import operator\nprint(operator.not_(True))" "False\n"

-- operator.itemgetter
#eval assertPy "from operator import itemgetter\ngetter = itemgetter('name')\nprint(getter({'name': 'Alice', 'age': 30}))" "Alice\n"
#eval assertPy "from operator import itemgetter\ngetter = itemgetter(1)\nprint(getter([10, 20, 30]))" "20\n"

-- operator.attrgetter
#eval assertPy "from operator import attrgetter\nclass Pt:\n    def __init__(self, x):\n        self.x = x\ngetter = attrgetter('x')\nprint(getter(Pt(42)))" "42\n"

-- ============================================================
-- functools stubs
-- ============================================================

-- lru_cache as decorator (identity)
#eval assertPy "from functools import lru_cache\n@lru_cache\ndef f(x):\n    return x * 2\nprint(f(5))" "10\n"

-- wraps as decorator (identity)
#eval assertPy "from functools import wraps\ndef decorator(f):\n    @wraps(f)\n    def wrapper(*args):\n        return f(*args)\n    return wrapper\n@decorator\ndef hello():\n    return 42\nprint(hello())" "42\n"

-- ============================================================
-- abc module tests
-- ============================================================

-- abstractmethod as pass-through decorator
#eval assertPy "from abc import abstractmethod\nclass Base:\n    @abstractmethod\n    def foo(self):\n        pass\nclass Sub(Base):\n    def foo(self):\n        return 42\nprint(Sub().foo())" "42\n"

-- ============================================================
-- functools.reduce
-- ============================================================

#eval assertPy "from functools import reduce\nprint(reduce(lambda a, b: a + b, [1, 2, 3, 4]))" "10\n"
#eval assertPy "from functools import reduce\nprint(reduce(lambda a, b: a * b, [1, 2, 3, 4]))" "24\n"
#eval assertPy "from functools import reduce\nprint(reduce(lambda a, b: a + b, [1, 2, 3], 10))" "16\n"
#eval assertPyError "from functools import reduce\nreduce(lambda a, b: a + b, [])" "reduce() of empty iterable"

-- ============================================================
-- itertools tests
-- ============================================================

-- itertools.chain
#eval assertPy "from itertools import chain\nresult = list(chain([1, 2], [3, 4]))\nprint(result)" "[1, 2, 3, 4]\n"
#eval assertPy "from itertools import chain\nresult = list(chain([1], [2], [3]))\nprint(result)" "[1, 2, 3]\n"

-- itertools.accumulate
#eval assertPy "from itertools import accumulate\nprint(list(accumulate([1, 2, 3, 4])))" "[1, 3, 6, 10]\n"
#eval assertPy "from itertools import accumulate\nprint(list(accumulate([1, 2, 3, 4], lambda a, b: a * b)))" "[1, 2, 6, 24]\n"
#eval assertPy "from itertools import accumulate\nprint(list(accumulate([1, 2, 3], initial=10)))" "[10, 11, 13, 16]\n"

-- ============================================================
-- collections tests
-- ============================================================

-- collections.defaultdict
#eval assertPy "from collections import defaultdict\nd = defaultdict(int)\nd['a']\nprint(d['a'])" "0\n"
#eval assertPy "from collections import defaultdict\nd = defaultdict(list)\nd['a'].append(1)\nprint(d['a'])" "[1]\n"
#eval assertPy "from collections import defaultdict\nd = defaultdict(int)\nd['x'] = 5\nprint(d['x'])" "5\n"

-- collections.OrderedDict (alias for dict)
#eval assertPy "from collections import OrderedDict\nd = OrderedDict()\nd['a'] = 1\nd['b'] = 2\nprint(list(d.keys()))" "['a', 'b']\n"

-- collections.abc import
#eval assertPy "from collections.abc import Mapping\nprint(Mapping)" "None\n"

-- ============================================================
-- enum module tests
-- ============================================================

-- Basic Enum
#eval assertPy "from enum import Enum\nclass Color(Enum):\n    RED = 1\n    GREEN = 2\n    BLUE = 3\nprint(Color.RED.value)\nprint(Color.RED.name)" "1\nRED\n"

-- Enum member access
#eval assertPy "from enum import Enum\nclass Color(Enum):\n    RED = 1\n    GREEN = 2\nprint(Color.GREEN.value)" "2\n"

-- IntEnum
#eval assertPy "from enum import IntEnum\nclass Priority(IntEnum):\n    LOW = 1\n    HIGH = 2\nprint(Priority.LOW.value)\nprint(Priority.HIGH.name)" "1\nHIGH\n"

-- auto()
#eval assertPy "from enum import Enum, auto\nclass Color(Enum):\n    RED = auto()\n    GREEN = auto()\n    BLUE = auto()\nprint(Color.RED.value)\nprint(Color.GREEN.value)\nprint(Color.BLUE.value)" "1\n2\n3\n"

-- ============================================================
-- abc module tests (real class)
-- ============================================================

-- ABC as base class
#eval assertPy "from abc import ABC, abstractmethod\nclass Animal(ABC):\n    @abstractmethod\n    def speak(self):\n        pass\nclass Dog(Animal):\n    def speak(self):\n        return 'Woof'\nprint(Dog().speak())" "Woof\n"

-- ============================================================
-- typing.get_type_hints
-- ============================================================

#eval assertPy "from typing import get_type_hints\nclass Foo:\n    x: int = 1\n    y: str = 'hello'\nhints = get_type_hints(Foo)\nprint(type(hints))" "<class 'dict'>\n"

-- ============================================================
-- Additional edge cases
-- ============================================================

-- math with negative numbers
#eval assertPy "import math\nprint(math.ceil(-0.5))" "0\n"
#eval assertPy "import math\nprint(math.floor(-0.5))" "-1\n"

-- functools.reduce with strings
#eval assertPy "from functools import reduce\nprint(reduce(lambda a, b: a + ' ' + b, ['hello', 'world']))" "hello world\n"

-- defaultdict nested
#eval assertPy "from collections import defaultdict\nd = defaultdict(int)\nd['a'] = 1\nd['b'] = 2\nprint(d['a'] + d['b'] + d['c'])" "3\n"

-- ============================================================
-- struct module tests
-- ============================================================

-- struct.calcsize
#eval assertPy "import struct\nprint(struct.calcsize('>H'))" "2\n"
#eval assertPy "import struct\nprint(struct.calcsize('>Q'))" "8\n"
#eval assertPy "import struct\nprint(struct.calcsize('>HQ'))" "10\n"
#eval assertPy "import struct\nprint(struct.calcsize('<BHI'))" "7\n"
#eval assertPy "import struct\nprint(struct.calcsize('<?'))" "1\n"

-- struct.pack + unpack round-trip (big-endian uint16)
#eval assertPy "import struct\ndata = struct.pack('>H', 256)\nprint(struct.unpack('>H', data)[0])" "256\n"

-- struct.pack + unpack round-trip (big-endian uint64)
#eval assertPy "import struct\ndata = struct.pack('>Q', 42)\nprint(struct.unpack('>Q', data)[0])" "42\n"

-- struct.pack + unpack round-trip (little-endian uint32)
#eval assertPy "import struct\ndata = struct.pack('<I', 12345)\nprint(struct.unpack('<I', data)[0])" "12345\n"

-- struct.pack bool
#eval assertPy "import struct\ndata = struct.pack('<?', True)\nprint(struct.unpack('<?', data)[0])" "True\n"

-- struct.pack uint8
#eval assertPy "import struct\ndata = struct.pack('>B', 255)\nprint(struct.unpack('>B', data)[0])" "255\n"

-- struct.pack multiple values
#eval assertPy "import struct\ndata = struct.pack('>BH', 1, 1000)\nresult = struct.unpack('>BH', data)\nprint(result[0])\nprint(result[1])" "1\n1000\n"

-- from struct import
#eval assertPy "from struct import pack, unpack\nprint(unpack('>H', pack('>H', 500))[0])" "500\n"

-- ============================================================
-- io module tests
-- ============================================================

-- BytesIO basic write + getvalue
#eval assertPy "import io\nb = io.BytesIO()\nb.write(b'\\x01\\x02\\x03')\nprint(len(b.getvalue()))" "3\n"

-- BytesIO write then seek and read
#eval assertPy "import io\nb = io.BytesIO()\nb.write(b'hello')\nb.seek(0)\ndata = b.read()\nprint(len(data))" "5\n"

-- BytesIO initial data
#eval assertPy "import io\nb = io.BytesIO(b'abc')\nprint(len(b.getvalue()))" "3\n"

-- BytesIO tell
#eval assertPy "import io\nb = io.BytesIO()\nb.write(b'hello')\nprint(b.tell())" "5\n"

-- BytesIO seek and read partial
#eval assertPy "import io\nb = io.BytesIO(b'\\x00\\x01\\x02\\x03\\x04')\ndata = b.read(3)\nprint(len(data))\nprint(b.tell())" "3\n3\n"

-- BytesIO context manager
#eval assertPy "import io\nwith io.BytesIO() as buf:\n    buf.write(b'test')\n    v = buf.getvalue()\nprint(len(v))" "4\n"

-- StringIO basic
#eval assertPy "import io\ns = io.StringIO()\ns.write('hello')\nprint(s.getvalue())" "hello\n"

-- StringIO seek and read
#eval assertPy "import io\ns = io.StringIO('hello world')\nresult = s.read(5)\nprint(result)" "hello\n"

-- from io import
#eval assertPy "from io import BytesIO\nb = BytesIO(b'\\x41\\x42')\nprint(len(b.getvalue()))" "2\n"

-- ============================================================
-- bisect module tests
-- ============================================================

-- bisect_left
#eval assertPy "import bisect\nprint(bisect.bisect_left([1, 3, 5, 7], 4))" "2\n"
#eval assertPy "import bisect\nprint(bisect.bisect_left([1, 3, 5, 7], 5))" "2\n"
#eval assertPy "import bisect\nprint(bisect.bisect_left([1, 3, 5, 7], 0))" "0\n"
#eval assertPy "import bisect\nprint(bisect.bisect_left([1, 3, 5, 7], 8))" "4\n"

-- bisect_right
#eval assertPy "import bisect\nprint(bisect.bisect_right([1, 3, 5, 7], 5))" "3\n"
#eval assertPy "import bisect\nprint(bisect.bisect_right([1, 3, 5, 7], 0))" "0\n"
#eval assertPy "import bisect\nprint(bisect.bisect_right([1, 3, 5, 7], 8))" "4\n"

-- insort
#eval assertPy "import bisect\na = [1, 3, 5]\nbisect.insort(a, 4)\nprint(a)" "[1, 3, 4, 5]\n"
#eval assertPy "import bisect\na = [1, 3, 5]\nbisect.insort(a, 0)\nprint(a)" "[0, 1, 3, 5]\n"
#eval assertPy "import bisect\na = [1, 3, 5]\nbisect.insort(a, 6)\nprint(a)" "[1, 3, 5, 6]\n"

-- from bisect import
#eval assertPy "from bisect import bisect_left, insort\na = [10, 20, 30]\nprint(bisect_left(a, 25))\ninsort(a, 25)\nprint(a)" "2\n[10, 20, 25, 30]\n"

-- ============================================================
-- base64 module tests
-- ============================================================

-- b64encode produces correct ASCII output
#eval assertPy "import base64\nresult = base64.b64encode(b'hello')\nprint(result.decode())" "aGVsbG8=\n"

-- b64decode round-trip
#eval assertPy "import base64\noriginal = b'hello world'\nencoded = base64.b64encode(original)\ndecoded = base64.b64decode(encoded)\nprint(decoded == original)" "True\n"

-- b16encode (hex encoding)
#eval assertPy "import base64\nresult = base64.b16encode(b'\\xff\\x00')\nprint(result.decode())" "FF00\n"

-- b16decode round-trip
#eval assertPy "import base64\noriginal = b'\\xab\\xcd\\xef'\nencoded = base64.b16encode(original)\ndecoded = base64.b16decode(encoded)\nprint(decoded == original)" "True\n"

-- urlsafe variants round-trip
#eval assertPy "import base64\noriginal = b'\\xfb\\xff\\xfe'\nencoded = base64.urlsafe_b64encode(original)\ndecoded = base64.urlsafe_b64decode(encoded)\nprint(decoded == original)" "True\n"

-- b64encode empty
#eval assertPy "import base64\nprint(base64.b64encode(b'').decode())" "\n"

-- from base64 import
#eval assertPy "from base64 import b64encode, b64decode\nprint(b64decode(b64encode(b'test')) == b'test')" "True\n"

-- ============================================================
-- json module tests
-- ============================================================

-- json.dumps primitives
#eval assertPy "import json\nprint(json.dumps(None))" "null\n"
#eval assertPy "import json\nprint(json.dumps(True))" "true\n"
#eval assertPy "import json\nprint(json.dumps(False))" "false\n"
#eval assertPy "import json\nprint(json.dumps(42))" "42\n"

-- json.dumps strings
#eval assertPy "import json\nprint(json.dumps('hello'))" "\"hello\"\n"

-- json.dumps list
#eval assertPy "import json\nprint(json.dumps([1, 2, 3]))" "[1, 2, 3]\n"

-- json.dumps dict
#eval assertPy "import json\nd = {'a': 1, 'b': 2}\nresult = json.dumps(d)\nprint('\"a\"' in result)\nprint('\"b\"' in result)" "True\nTrue\n"

-- json.loads primitives
#eval assertPy "import json\nprint(json.loads('null'))" "None\n"
#eval assertPy "import json\nprint(json.loads('true'))" "True\n"
#eval assertPy "import json\nprint(json.loads('false'))" "False\n"
#eval assertPy "import json\nprint(json.loads('42'))" "42\n"

-- json.loads string
#eval assertPy "import json\nprint(json.loads('\"hello\"'))" "hello\n"

-- json.loads array
#eval assertPy "import json\nresult = json.loads('[1, 2, 3]')\nprint(len(result))\nprint(result[0])\nprint(result[2])" "3\n1\n3\n"

-- json.loads object
#eval assertPy "import json\nresult = json.loads('{\"x\": 42}')\nprint(result['x'])" "42\n"

-- json round-trip
#eval assertPy "import json\noriginal = {'key': [1, 2, 3], 'flag': True, 'name': 'test'}\ns = json.dumps(original)\nparsed = json.loads(s)\nprint(parsed['key'][0])\nprint(parsed['flag'])\nprint(parsed['name'])" "1\nTrue\ntest\n"

-- from json import
#eval assertPy "from json import dumps, loads\nprint(loads(dumps([1, 2]))[1])" "2\n"

-- ============================================================
-- hashlib module tests
-- ============================================================

-- sha256 empty string
#eval assertPy "import hashlib\nprint(hashlib.sha256(b'').hexdigest())" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855\n"

-- sha256 'abc' (NIST test vector)
#eval assertPy "import hashlib\nprint(hashlib.sha256(b'abc').hexdigest())" "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad\n"

-- sha256 'hello'
#eval assertPy "import hashlib\nprint(hashlib.sha256(b'hello').hexdigest())" "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824\n"

-- sha256 incremental update equals one-shot
#eval assertPy "import hashlib\nh = hashlib.sha256()\nh.update(b'hel')\nh.update(b'lo')\nprint(h.hexdigest())" "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824\n"

-- sha256 digest returns 32 bytes
#eval assertPy "import hashlib\nprint(len(hashlib.sha256(b'hello').digest()))" "32\n"

-- sha256 digest_size attribute
#eval assertPy "import hashlib\nprint(hashlib.sha256().digest_size)" "32\n"

-- sha256 name attribute
#eval assertPy "import hashlib\nprint(hashlib.sha256().name)" "sha256\n"

-- sha256 multi-block input (NIST 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq')
#eval assertPy "import hashlib\nprint(hashlib.sha256(b'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq').hexdigest())" "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1\n"

-- hashlib.new generic constructor
#eval assertPy "import hashlib\nh = hashlib.new('sha256', b'hello')\nprint(h.hexdigest())" "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824\n"

-- from hashlib import
#eval assertPy "from hashlib import sha256\nprint(sha256(b'test').hexdigest())" "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08\n"

-- ============================================================
-- shake_128 tests
-- ============================================================

-- shake_128 output length
#eval assertPy "import hashlib\nh = hashlib.shake_128(b'hello')\nresult = h.digest(16)\nprint(len(result))" "16\n"

-- shake_128 output length 32
#eval assertPy "import hashlib\nresult = hashlib.shake_128(b'').digest(32)\nprint(len(result))" "32\n"

-- ============================================================
-- hmac module tests
-- ============================================================

-- hmac.new with sha256 - RFC 4231 Test Case 2
#eval assertPy "import hmac\nimport hashlib\nresult = hmac.new(b'Jefe', b'what do ya want for nothing?', hashlib.sha256).hexdigest()\nprint(result)" "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843\n"

-- hmac digest length
#eval assertPy "import hmac\nimport hashlib\nresult = hmac.new(b'key', b'message', hashlib.sha256).digest()\nprint(len(result))" "32\n"

-- ============================================================
-- secrets module tests
-- ============================================================

-- secrets.token_bytes returns bytes of correct length
#eval assertPy "import secrets\nresult = secrets.token_bytes(32)\nprint(len(result))\nprint(type(result))" "32\n<class 'bytes'>\n"

-- secrets.token_bytes default length
#eval assertPy "import secrets\nresult = secrets.token_bytes()\nprint(len(result))" "32\n"

-- secrets.randbelow returns int in range
#eval assertPy "import secrets\nfor _ in range(10):\n    x = secrets.randbelow(100)\n    assert 0 <= x < 100\nprint('ok')" "ok\n"

-- ============================================================
-- sys module tests
-- ============================================================

-- sys.path is a list
#eval assertPy "import sys\nprint(type(sys.path))" "<class 'list'>\n"

-- sys.argv is a list
#eval assertPy "import sys\nprint(type(sys.argv))" "<class 'list'>\n"

-- sys.version is a string
#eval assertPy "import sys\nprint(type(sys.version))" "<class 'str'>\n"

-- sys.maxsize is a positive integer
#eval assertPy "import sys\nprint(sys.maxsize > 0)" "True\n"

-- sys.platform is a string
#eval assertPy "import sys\nprint(type(sys.platform))" "<class 'str'>\n"

-- sys.version_info is a tuple
#eval assertPy "import sys\nprint(sys.version_info[0])" "3\n"

-- sys.byteorder
#eval assertPy "import sys\nprint(sys.byteorder)" "little\n"

-- sys.stdout.write
#eval assertPy "import sys\nsys.stdout.write('hello')\nprint()" "hello\n"

-- sys.exit raises SystemExit
#eval assertPyError "import sys\nsys.exit(1)" "SystemExit"

-- sys.getrecursionlimit
#eval assertPy "import sys\nprint(sys.getrecursionlimit() > 0)" "True\n"

-- ============================================================
-- os module tests
-- ============================================================

-- os.getcwd returns a string
#eval assertPy "import os\nprint(type(os.getcwd()))" "<class 'str'>\n"

-- os.sep
#eval assertPy "import os\nprint(os.sep)" "/\n"

-- os.name
#eval assertPy "import os\nprint(os.name)" "posix\n"

-- os.path.join
#eval assertPy "import os\nprint(os.path.join('a', 'b'))" "a/b\n"
#eval assertPy "import os\nprint(os.path.join('a', 'b', 'c'))" "a/b/c\n"
#eval assertPy "import os\nprint(os.path.join('a', '/b'))" "/b\n"

-- os.path.basename
#eval assertPy "import os\nprint(os.path.basename('/foo/bar.py'))" "bar.py\n"

-- os.path.dirname
#eval assertPy "import os\nprint(os.path.dirname('/foo/bar.py'))" "/foo\n"

-- os.path.splitext
#eval assertPy "import os\nprint(os.path.splitext('test.py'))" "('test', '.py')\n"
#eval assertPy "import os\nprint(os.path.splitext('noext'))" "('noext', '')\n"

-- os.path submodule import
#eval assertPy "import os.path\nprint(os.path.join('x', 'y'))" "x/y\n"

-- os.path.exists on current directory
#eval assertPy "import os\nprint(os.path.isdir('.'))" "True\n"

-- ============================================================
-- time module tests
-- ============================================================

-- time.time returns positive float
#eval assertPy "import time\nt = time.time()\nprint(t > 0)" "True\n"

-- time.monotonic returns non-negative float
#eval assertPy "import time\nt = time.monotonic()\nprint(t >= 0)" "True\n"

-- time.time returns float type
#eval assertPy "import time\nprint(type(time.time()))" "<class 'float'>\n"

-- ============================================================
-- datetime module tests
-- ============================================================

-- timedelta total_seconds
#eval assertPy "from datetime import timedelta\nd = timedelta(days=1)\nprint(d.total_seconds())" "86400.000000\n"

-- timedelta with seconds
#eval assertPy "from datetime import timedelta\nd = timedelta(seconds=3661)\nprint(d.total_seconds())" "3661.000000\n"

-- timedelta with days and seconds
#eval assertPy "from datetime import timedelta\nd = timedelta(days=1, seconds=30)\nprint(d.total_seconds())" "86430.000000\n"

-- datetime constructor
#eval assertPy "from datetime import datetime\ndt = datetime(2024, 1, 15)\nprint(dt.isoformat())" "2024-01-15T00:00:00\n"

-- datetime with time
#eval assertPy "from datetime import datetime\ndt = datetime(2024, 1, 15, 10, 30, 45)\nprint(dt.isoformat())" "2024-01-15T10:30:45\n"

-- timezone.utc
#eval assertPy "from datetime import timezone\nprint(timezone.utc)" "UTC\n"

-- ============================================================
-- pathlib module tests
-- ============================================================

-- Path name
#eval assertPy "from pathlib import Path\np = Path('/foo/bar.txt')\nprint(p.name)" "bar.txt\n"

-- Path suffix
#eval assertPy "from pathlib import Path\np = Path('/foo/bar.txt')\nprint(p.suffix)" ".txt\n"

-- Path stem
#eval assertPy "from pathlib import Path\np = Path('/foo/bar.txt')\nprint(p.stem)" "bar\n"

-- Path parent
#eval assertPy "from pathlib import Path\np = Path('/foo/bar.txt')\nprint(p.parent)" "/foo\n"

-- Path / operator
#eval assertPy "from pathlib import Path\np = Path('/foo') / 'bar'\nprint(p)" "/foo/bar\n"

-- Path str
#eval assertPy "from pathlib import Path\np = Path('/foo/bar')\nprint(str(p))" "/foo/bar\n"

-- Path exists on current dir
#eval assertPy "from pathlib import Path\np = Path('.')\nprint(p.is_dir())" "True\n"

-- ============================================================
-- logging module tests
-- ============================================================

-- getLogger returns Logger instance
#eval assertPy "import logging\nlogger = logging.getLogger('test')\nprint(type(logger))" "<class 'Logger'>\n"

-- basicConfig is no-op
#eval assertPy "import logging\nlogging.basicConfig()\nprint('ok')" "ok\n"

-- Logger level constants
#eval assertPy "import logging\nprint(logging.DEBUG)" "10\n"
#eval assertPy "import logging\nprint(logging.WARNING)" "30\n"

-- Logger warning emits output
#eval assertPy "import logging\nlogger = logging.getLogger('test')\nlogger.warning('hello')" "WARNING:test:hello\n"

-- Logger debug suppressed by default (level=WARNING)
#eval assertPy "import logging\nlogger = logging.getLogger('test')\nlogger.debug('hidden')\nprint('ok')" "ok\n"

-- Logger setLevel
#eval assertPy "import logging\nlogger = logging.getLogger('test')\nlogger.setLevel(logging.DEBUG)\nlogger.debug('visible')" "DEBUG:test:visible\n"

-- ============================================================
-- signal module tests
-- ============================================================

-- signal constants
#eval assertPy "import signal\nprint(signal.SIGINT)" "2\n"
#eval assertPy "import signal\nprint(signal.SIGTERM)" "15\n"

-- signal.signal is no-op returning handler
#eval assertPy "import signal\ndef handler(s, f):\n    pass\nresult = signal.signal(signal.SIGINT, handler)\nprint('ok')" "ok\n"

-- ============================================================
-- threading module tests
-- ============================================================

-- Lock acquire/release
#eval assertPy "import threading\nlock = threading.Lock()\nlock.acquire()\nlock.release()\nprint('ok')" "ok\n"

-- Lock as context manager
#eval assertPy "import threading\nlock = threading.Lock()\nwith lock:\n    print('locked')" "locked\n"

-- RLock
#eval assertPy "import threading\nlock = threading.RLock()\nlock.acquire()\nlock.release()\nprint('ok')" "ok\n"

-- ============================================================
-- tempfile module tests
-- ============================================================

-- mkdtemp returns string
#eval assertPy "import tempfile\nd = tempfile.mkdtemp()\nprint(type(d))" "<class 'str'>\n"

-- mkdtemp returns path starting with /tmp
#eval assertPy "import tempfile\nd = tempfile.mkdtemp()\nprint(d.startswith('/tmp'))" "True\n"

-- NamedTemporaryFile has name attribute
#eval assertPy "import tempfile\nf = tempfile.NamedTemporaryFile()\nprint(type(f.name))" "<class 'str'>\n"

-- ============================================================
-- pydantic module tests
-- ============================================================

-- Basic model definition and field access
#eval assertPy "from pydantic import BaseModel\nclass Point(BaseModel):\n    x: int\n    y: int\np = Point(x=1, y=2)\nprint(p.x)\nprint(p.y)" "1\n2\n"

-- __repr__
#eval assertPy "from pydantic import BaseModel\nclass Point(BaseModel):\n    x: int\n    y: int\np = Point(x=1, y=2)\nprint(repr(p))" "Point(x=1, y=2)\n"

-- __eq__
#eval assertPy "from pydantic import BaseModel\nclass Point(BaseModel):\n    x: int\n    y: int\nprint(Point(x=1, y=2) == Point(x=1, y=2))\nprint(Point(x=1, y=2) == Point(x=1, y=3))" "True\nFalse\n"

-- Frozen model via ConfigDict
#eval assertPy "from pydantic import BaseModel, ConfigDict\nclass Frozen(BaseModel):\n    model_config = ConfigDict(frozen=True)\n    x: int\nf = Frozen(x=5)\nprint(f.x)" "5\n"

-- model_copy with update kwarg
#eval assertPy "from pydantic import BaseModel, ConfigDict\nclass State(BaseModel):\n    model_config = ConfigDict(frozen=True)\n    slot: int\n    balance: int\ns = State(slot=0, balance=100)\ns2 = s.model_copy(update={'slot': 1})\nprint(s2.slot)\nprint(s2.balance)" "1\n100\n"

-- model_dump
#eval assertPy "from pydantic import BaseModel, ConfigDict\nclass Frozen(BaseModel):\n    model_config = ConfigDict(frozen=True)\n    x: int\nf = Frozen(x=5)\nd = f.model_dump()\nprint(d['x'])" "5\n"

-- model_fields class attribute
#eval assertPy "from pydantic import BaseModel\nclass Point(BaseModel):\n    x: int\n    y: int\nprint('x' in Point.model_fields)\nprint('z' in Point.model_fields)" "True\nFalse\n"

-- Field defaults
#eval assertPy "from pydantic import BaseModel\nclass WithDefault(BaseModel):\n    x: int\n    y: int = 42\nw = WithDefault(x=1)\nprint(w.x)\nprint(w.y)" "1\n42\n"

-- Inheritance: child inherits parent fields
#eval assertPy "from pydantic import BaseModel\nclass Base(BaseModel):\n    x: int\nclass Child(Base):\n    y: int\nc = Child(x=1, y=2)\nprint(c.x)\nprint(c.y)" "1\n2\n"

-- __hash__ for frozen models
#eval assertPy "from pydantic import BaseModel, ConfigDict\nclass Frozen(BaseModel):\n    model_config = ConfigDict(frozen=True)\n    x: int\nf = Frozen(x=5)\nprint(type(hash(f)))" "<class 'int'>\n"

-- ============================================================
-- pydantic Phase 8b: validators, serializers, core_schema
-- ============================================================

-- field_validator(mode="before") transforms input
#eval assertPy "from pydantic import BaseModel, field_validator\nclass Doubled(BaseModel):\n    x: int\n    @field_validator('x', mode='before')\n    @classmethod\n    def double_x(cls, v):\n        return v * 2\nd = Doubled(x=5)\nprint(d.x)" "10\n"

-- model_validator(mode="after") validates constructed instance
#eval assertPy "from pydantic import BaseModel, model_validator\nclass Checked(BaseModel):\n    x: int\n    @model_validator(mode='after')\n    def check(self):\n        return self\nc = Checked(x=5)\nprint(c.x)" "5\n"

-- model_validator(mode="before") transforms input dict
#eval assertPy "from pydantic import BaseModel, model_validator\nclass Swapped(BaseModel):\n    a: int\n    b: int\n    @model_validator(mode='before')\n    @classmethod\n    def swap(cls, data):\n        tmp = data['a']\n        data['a'] = data['b']\n        data['b'] = tmp\n        return data\ns = Swapped(a=1, b=2)\nprint(s.a)\nprint(s.b)" "2\n1\n"

-- field_serializer with model_dump(mode="json")
#eval assertPy "from pydantic import BaseModel, field_serializer\nclass Hex(BaseModel):\n    v: int\n    @field_serializer('v', when_used='json')\n    def hex_v(self, v, _info):\n        return hex(v)\nh = Hex(v=255)\nd = h.model_dump(mode='json')\nprint(d['v'])" "0xff\n"

-- model_dump without mode="json" does not apply field_serializer
#eval assertPy "from pydantic import BaseModel, field_serializer\nclass Hex(BaseModel):\n    v: int\n    @field_serializer('v', when_used='json')\n    def hex_v(self, v, _info):\n        return hex(v)\nh = Hex(v=255)\nd = h.model_dump()\nprint(d['v'])" "255\n"

-- pydantic_core.core_schema module import
#eval assertPy "from pydantic_core import core_schema\nprint(type(core_schema))" "<class 'module'>\n"

-- core_schema.int_schema returns dict with type
#eval assertPy "from pydantic_core import core_schema\ns = core_schema.int_schema(ge=0, lt=256)\nprint(s['type'])\nprint(s['ge'])\nprint(s['lt'])" "int\n0\n256\n"

-- core_schema.union_schema returns dict
#eval assertPy "from pydantic_core import core_schema\ns = core_schema.union_schema([core_schema.int_schema()])\nprint(s['type'])" "union\n"

-- core_schema.is_instance_schema returns dict
#eval assertPy "from pydantic_core import core_schema\ns = core_schema.is_instance_schema(int)\nprint(s['type'])" "is-instance\n"

-- core_schema.chain_schema returns dict
#eval assertPy "from pydantic_core import core_schema\ns = core_schema.chain_schema([core_schema.int_schema()])\nprint(s['type'])" "chain\n"

-- ============================================================
-- Phase 8c: Pydantic advanced features
-- ============================================================

-- extra="forbid" rejects unknown kwargs
#eval assertPyError "from pydantic import BaseModel, ConfigDict\nclass M(BaseModel):\n    model_config = ConfigDict(extra='forbid')\n    x: int = 0\nM(x=1, y=2)" "extra fields not permitted"

-- extra="forbid" accepts known fields
#eval assertPy "from pydantic import BaseModel, ConfigDict\nclass M(BaseModel):\n    model_config = ConfigDict(extra='forbid')\n    x: int = 0\nm = M(x=42)\nprint(m.x)" "42\n"

-- to_camel converts snake_case to camelCase
#eval assertPy "from pydantic.alias_generators import to_camel\nprint(to_camel('current_slot'))\nprint(to_camel('parent_root'))\nprint(to_camel('hello'))\nprint(to_camel('a_b_c'))" "currentSlot\nparentRoot\nhello\naBC\n"

-- alias_generator with model_dump(by_alias=True)
#eval assertPy "from pydantic import BaseModel, ConfigDict\nfrom pydantic.alias_generators import to_camel\nclass M(BaseModel):\n    model_config = ConfigDict(alias_generator=to_camel)\n    current_slot: int = 0\nm = M(current_slot=42)\nd = m.model_dump(by_alias=True)\nprint(d['currentSlot'])" "42\n"

-- model_dump without by_alias uses Python field names
#eval assertPy "from pydantic import BaseModel, ConfigDict\nfrom pydantic.alias_generators import to_camel\nclass M(BaseModel):\n    model_config = ConfigDict(alias_generator=to_camel)\n    current_slot: int = 0\nm = M(current_slot=42)\nd = m.model_dump()\nprint(d['current_slot'])" "42\n"

-- populate_by_name accepts both alias and field name
#eval assertPy "from pydantic import BaseModel, ConfigDict\nfrom pydantic.alias_generators import to_camel\nclass M(BaseModel):\n    model_config = ConfigDict(alias_generator=to_camel, populate_by_name=True)\n    current_slot: int = 0\nm = M(currentSlot=42)\nprint(m.current_slot)" "42\n"

-- ConfigDict merge via | operator (leanSpec pattern)
#eval assertPy "from pydantic import BaseModel, ConfigDict\nfrom pydantic.alias_generators import to_camel\nclass CamelModel(BaseModel):\n    model_config = ConfigDict(alias_generator=to_camel, populate_by_name=True)\nclass StrictModel(CamelModel):\n    model_config = CamelModel.model_config | {'extra': 'forbid', 'frozen': True}\n    x: int = 0\nm = StrictModel(x=5)\nprint(m.x)\nd = m.model_dump(by_alias=True)\nprint(d['x'])" "5\n5\n"

-- Inheritance: alias_generator inherited from parent
#eval assertPy "from pydantic import BaseModel, ConfigDict\nfrom pydantic.alias_generators import to_camel\nclass CamelModel(BaseModel):\n    model_config = ConfigDict(alias_generator=to_camel, populate_by_name=True)\nclass Child(CamelModel):\n    my_field: int = 0\nm = Child(my_field=10)\nd = m.model_dump(by_alias=True)\nprint(d['myField'])" "10\n"

-- __get_pydantic_core_schema__ hook validates field values
#eval assertPy "from pydantic import BaseModel\nfrom pydantic_core import core_schema\ndef make_positive(v):\n    if v < 0:\n        raise ValueError('must be positive')\n    return v\nclass Positive:\n    @classmethod\n    def __get_pydantic_core_schema__(cls, source_type, handler):\n        return core_schema.no_info_plain_validator_function(make_positive)\nclass M(BaseModel):\n    value: Positive = 0\nm = M(value=42)\nprint(m.value)" "42\n"

-- __get_pydantic_core_schema__ hook rejects invalid values
#eval assertPyError "from pydantic import BaseModel\nfrom pydantic_core import core_schema\ndef make_positive(v):\n    if v < 0:\n        raise ValueError('must be positive')\n    return v\nclass Positive:\n    @classmethod\n    def __get_pydantic_core_schema__(cls, source_type, handler):\n        return core_schema.no_info_plain_validator_function(make_positive)\nclass M(BaseModel):\n    value: Positive = 0\nM(value=-5)" "must be positive"

-- __get_pydantic_core_schema__ with int_schema constraints
#eval assertPy "from pydantic import BaseModel\nfrom pydantic_core import core_schema\nclass BoundedInt:\n    @classmethod\n    def __get_pydantic_core_schema__(cls, source_type, handler):\n        return core_schema.int_schema(ge=0, lt=100)\nclass M(BaseModel):\n    x: BoundedInt = 0\nm = M(x=50)\nprint(m.x)" "50\n"

-- __get_pydantic_core_schema__ rejects out-of-range
#eval assertPyError "from pydantic import BaseModel\nfrom pydantic_core import core_schema\nclass BoundedInt:\n    @classmethod\n    def __get_pydantic_core_schema__(cls, source_type, handler):\n        return core_schema.int_schema(ge=0, lt=100)\nclass M(BaseModel):\n    x: BoundedInt = 0\nM(x=200)" "200 >= maximum 100"

-- __get_pydantic_core_schema__ classmethod is callable and returns schema
#eval assertPy "from pydantic_core import core_schema\nclass BoundedInt:\n    @classmethod\n    def __get_pydantic_core_schema__(cls, source_type, handler):\n        return core_schema.int_schema(ge=0, lt=100)\nresult = BoundedInt.__get_pydantic_core_schema__(None, None)\nprint(result['type'])" "int\n"

-- ============================================================
-- Phase 9a: leanSpec primitive type support
-- ============================================================

-- Exception hierarchy
#eval assertPy "class SSZError(Exception):\n    pass\nclass SSZTypeError(SSZError):\n    pass\ntry:\n    raise SSZTypeError('bad type')\nexcept SSZError as e:\n    print(type(e).__name__)\n    print(str(e))" "SSZTypeError\nbad type\n"

-- ABC with classmethod + abstractmethod
#eval assertPy "from abc import ABC, abstractmethod\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\nclass Concrete(SSZType):\n    @classmethod\n    def is_fixed_size(cls):\n        return True\nprint(Concrete.is_fixed_size())" "True\n"

-- @override decorator (identity, doesn't fail)
#eval assertPy "from typing import override\ndef base():\n    return 1\n@override\ndef child():\n    return 2\nprint(child())" "2\n"

-- Multiple inheritance from int: basic construction
#eval assertPy "from abc import ABC, abstractmethod\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\nclass BaseUint(int, SSZType):\n    __slots__ = ()\n    BITS = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\nclass Uint64(BaseUint):\n    BITS = 64\nx = Uint64(42)\nprint(int(x))\nprint(type(x).__name__)" "42\nUint64\n"

-- isinstance checks for int subclass
#eval assertPy "from abc import ABC, abstractmethod\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\nclass BaseUint(int, SSZType):\n    __slots__ = ()\n    BITS = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\nclass Uint64(BaseUint):\n    BITS = 64\nx = Uint64(42)\nprint(isinstance(x, Uint64))\nprint(isinstance(x, BaseUint))\nprint(isinstance(x, int))\nprint(isinstance(x, SSZType))" "True\nTrue\nTrue\nTrue\n"

-- Arithmetic on int subclasses via dunder methods
#eval assertPy "class BaseUint(int):\n    BITS = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    def __add__(self, other):\n        return type(self)(super().__add__(other))\nclass Uint64(BaseUint):\n    BITS = 64\nx = Uint64(10)\ny = Uint64(20)\nz = x + y\nprint(int(z))\nprint(type(z).__name__)" "30\nUint64\n"

-- Comparison operators on int subclasses
#eval assertPy "class BaseUint(int):\n    BITS = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\nclass Uint64(BaseUint):\n    BITS = 64\na = Uint64(10)\nb = Uint64(20)\nprint(a < b)\nprint(a == Uint64(10))\nprint(a > b)" "True\nTrue\nFalse\n"

-- Range validation in __new__
#eval assertPyError "class SSZValueError(Exception):\n    pass\nclass BaseUint(int):\n    BITS = 8\n    def __new__(cls, value):\n        if not isinstance(value, int) or isinstance(value, bool):\n            raise TypeError('Expected int')\n        int_value = int(value)\n        max_value = 2 ** cls.BITS - 1\n        if not (0 <= int_value <= max_value):\n            raise SSZValueError('out of range')\n        return super().__new__(cls, int_value)\nclass Uint8(BaseUint):\n    BITS = 8\nUint8(256)" "out of range"

-- Negative value rejected
#eval assertPyError "class SSZValueError(Exception):\n    pass\nclass BaseUint(int):\n    BITS = 8\n    def __new__(cls, value):\n        if not isinstance(value, int) or isinstance(value, bool):\n            raise TypeError('Expected int')\n        int_value = int(value)\n        if not (0 <= int_value <= 2 ** cls.BITS - 1):\n            raise SSZValueError('out of range')\n        return super().__new__(cls, int_value)\nclass Uint8(BaseUint):\n    BITS = 8\nUint8(-1)" "out of range"

-- Bool rejected (isinstance(True, int) is True, but we check isinstance(value, bool))
#eval assertPyError "class BaseUint(int):\n    BITS = 8\n    def __new__(cls, value):\n        if not isinstance(value, int) or isinstance(value, bool):\n            raise TypeError('Expected int')\n        return super().__new__(cls, int(value))\nclass Uint8(BaseUint):\n    BITS = 8\nUint8(True)" "Expected int"

-- Bitwise operations on int subclasses
#eval assertPy "class BaseUint(int):\n    BITS = 8\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    def __and__(self, other):\n        return type(self)(super().__and__(other))\n    def __or__(self, other):\n        return type(self)(super().__or__(other))\n    def __xor__(self, other):\n        return type(self)(super().__xor__(other))\nclass Uint8(BaseUint):\n    BITS = 8\na = Uint8(0b1100)\nb = Uint8(0b1010)\nprint(int(a & b))\nprint(int(a | b))\nprint(int(a ^ b))" "8\n14\n6\n"

-- to_bytes on int subclass instance
#eval assertPy "class BaseUint(int):\n    BITS = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    def serialize_to_bytes(self):\n        return int(self).to_bytes(self.BITS // 8, 'little')\nclass Uint64(BaseUint):\n    BITS = 64\nx = Uint64(256)\nb = x.serialize_to_bytes()\nprint(len(b))\nprint(b[0])\nprint(b[1])" "8\n0\n1\n"

-- hash() works on int subclass instances (for Pydantic frozen models)
#eval assertPy "class BaseUint(int):\n    BITS = 8\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    def __hash__(self):\n        return hash((type(self).__name__, int(self)))\nclass Uint8(BaseUint):\n    BITS = 8\nx = Uint8(42)\nh = hash(x)\nprint(isinstance(h, int))\nprint(h != 0)" "True\nTrue\n"

-- ClassVar accessible on subclasses
#eval assertPy "class BaseUint(int):\n    BITS = 0\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\nclass Uint8(BaseUint):\n    BITS = 8\nclass Uint64(BaseUint):\n    BITS = 64\nprint(Uint8.BITS)\nprint(Uint64.BITS)" "8\n64\n"

-- Multiple Uint subclasses
#eval assertPy "class BaseUint(int):\n    BITS = 0\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\nclass Uint8(BaseUint):\n    BITS = 8\nclass Uint16(BaseUint):\n    BITS = 16\nclass Uint32(BaseUint):\n    BITS = 32\nclass Uint64(BaseUint):\n    BITS = 64\nprint(int(Uint8(255)))\nprint(int(Uint16(1000)))\nprint(int(Uint32(100000)))\nprint(int(Uint64(2**40)))" "255\n1000\n100000\n1099511627776\n"

-- Instance method calls with kwargs
#eval assertPy "class Foo:\n    def greet(self, name='world', greeting='hello'):\n        return greeting + ' ' + name\nf = Foo()\nprint(f.greet(greeting='hi', name='alice'))" "hi alice\n"

-- kwargs forwarded through super()
#eval assertPy "class Base:\n    def method(self, x=1, y=2):\n        return x + y\nclass Child(Base):\n    def method(self, x=1, y=2):\n        return super().method(x=x, y=y) * 10\nc = Child()\nprint(c.method(x=3, y=4))" "70\n"

-- bytes * int multiplication
#eval assertPy "b = b'\\x00' * 32\nprint(len(b))\nprint(b[0])\nb2 = b'\\xab' * 4\nprint(len(b2))\nprint(b2[0])" "32\n0\n4\n171\n"

-- bytes(instance) with wrappedValue
#eval assertPy "class BaseBytes(bytes):\n    def __new__(cls, value=b''):\n        return super().__new__(cls, value)\nclass Bytes4(BaseBytes):\n    pass\nx = Bytes4(b'\\x01\\x02\\x03\\x04')\nraw = bytes(x)\nprint(len(raw))\nprint(raw[0])\nprint(raw[3])" "4\n1\n4\n"

-- bytes(self).hex() conversion chain
#eval assertPy "class BaseBytes(bytes):\n    def __new__(cls, value=b''):\n        return super().__new__(cls, value)\nclass Bytes4(BaseBytes):\n    pass\nx = Bytes4(b'\\xde\\xad\\xbe\\xef')\nprint(bytes(x).hex())" "deadbeef\n"

-- pow(base, exp, None) = pow(base, exp)
#eval assertPy "print(pow(2, 10, None))\nprint(pow(3, 4, None))" "1024\n81\n"

-- Instance method calling another instance method
#eval assertPy "class BaseUint(int):\n    BITS = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    def _raise_type_error(self, other, op):\n        raise TypeError('bad: ' + op)\n    def __add__(self, other):\n        if not isinstance(other, BaseUint):\n            self._raise_type_error(other, '+')\n        return type(self)(int(self) + int(other))\nclass Uint64(BaseUint):\n    BITS = 64\ntry:\n    Uint64(1) + 'hello'\nexcept TypeError as e:\n    print(str(e))" "bad: +\n"

-- Classmethod calling cls() constructor with int.from_bytes
#eval assertPy "class BaseUint(int):\n    BITS = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def get_byte_length(cls):\n        return cls.BITS // 8\n    @classmethod\n    def decode_bytes(cls, data):\n        return cls(int.from_bytes(data, 'little'))\nclass Uint64(BaseUint):\n    BITS = 64\nresult = Uint64.decode_bytes(b'\\x00\\x01\\x00\\x00\\x00\\x00\\x00\\x00')\nprint(int(result))\nprint(type(result).__name__)" "256\nUint64\n"

-- to_bytes with keyword args on int subclass instance
#eval assertPy "class BaseUint(int):\n    BITS = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def get_byte_length(cls):\n        return cls.BITS // 8\n    def encode_bytes(self):\n        return int(self).to_bytes(length=self.get_byte_length(), byteorder='little')\nclass Uint64(BaseUint):\n    BITS = 64\nx = Uint64(256)\nb = x.encode_bytes()\nprint(len(b))\nprint(b[0])\nprint(b[1])" "8\n0\n1\n"

-- Full BaseUint encode/decode round-trip via io.BytesIO
#eval assertPy "import io\nclass BaseUint(int):\n    BITS = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def get_byte_length(cls):\n        return cls.BITS // 8\n    def encode_bytes(self):\n        return int(self).to_bytes(self.get_byte_length(), 'little')\n    @classmethod\n    def decode_bytes(cls, data):\n        return cls(int.from_bytes(data, 'little'))\n    def serialize(self, stream):\n        data = self.encode_bytes()\n        stream.write(data)\n        return len(data)\nclass Uint64(BaseUint):\n    BITS = 64\nx = Uint64(12345678)\nstream = io.BytesIO()\nx.serialize(stream)\nraw = stream.getvalue()\nprint(len(raw))\ny = Uint64.decode_bytes(raw)\nprint(int(y))\nprint(type(y).__name__)" "8\n12345678\nUint64\n"

-- BaseBytes subclass with length validation and coercion
#eval assertPy "class SSZValueError(Exception):\n    pass\nclass BaseBytes(bytes):\n    LENGTH = 0\n    def __new__(cls, value=b''):\n        if isinstance(value, (bytes, bytearray)):\n            b = bytes(value)\n        elif isinstance(value, str):\n            b = bytes.fromhex(value)\n        else:\n            raise TypeError('Cannot coerce')\n        if len(b) != cls.LENGTH:\n            raise SSZValueError('wrong length')\n        return super().__new__(cls, b)\n    @classmethod\n    def zero(cls):\n        return cls(b'\\x00' * cls.LENGTH)\nclass Bytes4(BaseBytes):\n    LENGTH = 4\nclass Bytes32(BaseBytes):\n    LENGTH = 32\nx = Bytes4(b'\\x01\\x02\\x03\\x04')\nprint(len(x))\nz = Bytes32.zero()\nprint(len(z))\nprint(z[0])" "4\n32\n0\n"

-- BaseBytes hex() via bytes(self).hex()
#eval assertPy "class BaseBytes(bytes):\n    LENGTH = 4\n    def __new__(cls, value=b''):\n        return super().__new__(cls, value)\n    def hex(self):\n        return bytes(self).hex()\n    def __repr__(self):\n        return type(self).__name__ + '(' + self.hex() + ')'\nclass Bytes4(BaseBytes):\n    LENGTH = 4\nx = Bytes4(b'\\xde\\xad\\xbe\\xef')\nprint(x.hex())\nprint(repr(x))" "deadbeef\nBytes4(deadbeef)\n"

-- hash() with tuple containing type(self)
#eval assertPy "class BaseUint(int):\n    BITS = 8\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    def __hash__(self):\n        return hash((type(self).__name__, int(self)))\nclass Uint8(BaseUint):\n    BITS = 8\na = Uint8(42)\nb = Uint8(42)\nprint(hash(a) == hash(b))" "True\n"

-- @classmethod + @override stacking (leanSpec pattern)
#eval assertPy "from abc import ABC, abstractmethod\nfrom typing import override\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n    @classmethod\n    @abstractmethod\n    def get_byte_length(cls):\n        pass\nclass BaseUint(int, SSZType):\n    __slots__ = ()\n    BITS = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    @override\n    def get_byte_length(cls):\n        return cls.BITS // 8\nclass Uint64(BaseUint):\n    BITS = 64\nprint(Uint64.is_fixed_size())\nprint(Uint64.get_byte_length())" "True\n8\n"

-- ============================================================
-- Phase 9a continued: int/bytes subclass interop with builtins
-- ============================================================

-- hex() on int subclass instance
#eval assertPy "class BaseUint(int):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\nclass Uint64(BaseUint):\n    pass\nprint(hex(Uint64(42)))\nprint(hex(Uint64(255)))" "0x2a\n0xff\n"

-- bin() on int subclass instance
#eval assertPy "class BaseUint(int):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\nclass Uint8(BaseUint):\n    pass\nprint(bin(Uint8(42)))" "0b101010\n"

-- oct() on int subclass instance
#eval assertPy "class BaseUint(int):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\nclass Uint8(BaseUint):\n    pass\nprint(oct(Uint8(42)))" "0o52\n"

-- range() with int subclass instances
#eval assertPy "class BaseUint(int):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\nclass Uint8(BaseUint):\n    pass\nresult = list(range(Uint8(5)))\nprint(result)" "[0, 1, 2, 3, 4]\n"

-- range(start, stop, step) with int subclass instances
#eval assertPy "class BaseUint(int):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\nclass Uint8(BaseUint):\n    pass\nresult = list(range(Uint8(2), Uint8(8), Uint8(2)))\nprint(result)" "[2, 4, 6]\n"

-- divmod() with int subclass instances
#eval assertPy "class BaseUint(int):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\nclass Uint64(BaseUint):\n    pass\nresult = divmod(Uint64(100), Uint64(3))\nprint(result[0])\nprint(result[1])" "33\n1\n"

-- List indexing with int subclass instance
#eval assertPy "class BaseUint(int):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\nclass Uint64(BaseUint):\n    pass\ndata = ['a', 'b', 'c', 'd', 'e']\nprint(data[Uint64(2)])\nprint(data[Uint64(0)])\nprint(data[Uint64(4)])" "c\na\ne\n"

-- Slice with int subclass instances
#eval assertPy "class BaseUint(int):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\nclass Uint8(BaseUint):\n    pass\ndata = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]\nprint(data[Uint8(2):Uint8(7)])" "[2, 3, 4, 5, 6]\n"

-- repr() and str() on int subclass via Python-defined __repr__/__str__
#eval assertPy "class BaseUint(int):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    def __repr__(self):\n        return type(self).__name__ + '(' + str(int(self)) + ')'\n    def __str__(self):\n        return str(int(self))\nclass Uint64(BaseUint):\n    pass\nx = Uint64(42)\nprint(repr(x))\nprint(str(x))" "Uint64(42)\n42\n"

-- int() conversion of int subclass instance
#eval assertPy "class BaseUint(int):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\nclass Uint64(BaseUint):\n    pass\nx = Uint64(42)\nresult = int(x)\nprint(result)\nprint(result == 42)" "42\nTrue\n"

-- bytes.__radd__: b"\xff" + bytes_subclass_instance
#eval assertPy "class BaseBytes(bytes):\n    def __new__(cls, value=b''):\n        return super().__new__(cls, value)\nclass Bytes4(BaseBytes):\n    pass\nx = Bytes4(b'\\x01\\x02\\x03\\x04')\nresult = b'\\xff' + x\nprint(len(result))\nprint(result[0])\nprint(result[4])" "5\n255\n4\n"

-- sorted() on bytes subclass instances (lexicographic)
#eval assertPy "class BaseBytes(bytes):\n    def __new__(cls, value=b''):\n        return super().__new__(cls, value)\nclass Bytes4(BaseBytes):\n    pass\na = Bytes4(b'\\x00\\x00\\x00\\x02')\nb = Bytes4(b'\\x00\\x00\\x00\\x01')\nc = Bytes4(b'\\xff\\xff\\xff\\xff')\nresult = sorted([c, a, b])\nprint(bytes(result[0]).hex())\nprint(bytes(result[1]).hex())\nprint(bytes(result[2]).hex())" "00000001\n00000002\nffffffff\n"

-- In-place operators: x += Uint64(5)
#eval assertPy "class BaseUint(int):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    def __add__(self, other):\n        return type(self)(int(self) + int(other))\nclass Uint64(BaseUint):\n    pass\nx = Uint64(10)\ny = x\nx += Uint64(5)\nprint(int(x))\nprint(type(x).__name__)\nprint(int(y))" "15\nUint64\n10\n"

-- Reverse operators: plain int + Uint raises TypeError when __radd__ checks type
#eval assertPy "class BaseUint(int):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    def __radd__(self, other):\n        if not isinstance(other, BaseUint):\n            raise TypeError('bad type for +')\n        return type(self)(int(other) + int(self))\nclass Uint64(BaseUint):\n    pass\ntry:\n    result = 100 + Uint64(3)\n    print('no error')\nexcept TypeError as e:\n    print(str(e))" "bad type for +\n"

-- io.BytesIO seek and read round-trip
#eval assertPy "import io\nstream = io.BytesIO()\nstream.write(b'\\x01\\x02\\x03\\x04')\nstream.seek(0)\ndata = stream.read()\nprint(len(data))\nprint(data[0])\nprint(data[3])" "4\n1\n4\n"

-- BaseBytes __add__ returns raw bytes
#eval assertPy "class BaseBytes(bytes):\n    def __new__(cls, value=b''):\n        return super().__new__(cls, value)\nclass Bytes4(BaseBytes):\n    pass\na = Bytes4(b'\\x01\\x02\\x03\\x04')\nb = Bytes4(b'\\x05\\x06\\x07\\x08')\nresult = a + b\nprint(len(result))\nprint(result[0])\nprint(result[7])" "8\n1\n8\n"

-- Boolean type: construction, validation, arithmetic rejection
#eval assertPy "class Boolean(int):\n    def __new__(cls, value):\n        if not isinstance(value, int):\n            raise TypeError('Expected bool or int')\n        if value not in (0, 1):\n            raise ValueError('Boolean value must be 0 or 1')\n        return super().__new__(cls, value)\n    def __add__(self, other):\n        raise TypeError('Arithmetic not supported for Boolean.')\n    def __eq__(self, other):\n        return isinstance(other, int) and int(self) == int(other)\nbt = Boolean(True)\nbf = Boolean(False)\nprint(int(bt))\nprint(int(bf))\nprint(bt == 1)\nprint(bf == 0)\ntry:\n    bt + bf\nexcept TypeError as e:\n    print(str(e))" "1\n0\nTrue\nTrue\nArithmetic not supported for Boolean.\n"

-- Boolean rejects invalid values
#eval assertPyError "class Boolean(int):\n    def __new__(cls, value):\n        if value not in (0, 1):\n            raise ValueError('must be 0 or 1')\n        return super().__new__(cls, value)\nBoolean(2)" "must be 0 or 1"
