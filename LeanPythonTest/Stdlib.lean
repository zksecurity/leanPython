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

-- ============================================================
-- Phase 9a: f-string !r / !s / !a conversion flags
-- ============================================================

-- f-string !r with int (repr same as str for ints)
#eval assertPy "print(f'{42!r}')" "42\n"

-- f-string !r with string (should add quotes)
#eval assertPy "print(f'{\"hello\"!r}')" "'hello'\n"

-- f-string !s with string (no quotes)
#eval assertPy "print(f'{\"hello\"!s}')" "hello\n"

-- f-string !r with None
#eval assertPy "print(f'{None!r}')" "None\n"

-- f-string !r with bool
#eval assertPy "print(f'{True!r}')" "True\n"

-- f-string !r with custom __repr__
#eval assertPy "class Foo:\n    def __init__(self, x):\n        self.x = x\n    def __repr__(self):\n        return 'Foo(' + str(self.x) + ')'\nf = Foo(42)\nprint(f'{f!r}')" "Foo(42)\n"

-- f-string !s with custom __str__
#eval assertPy "class Foo:\n    def __init__(self, x):\n        self.x = x\n    def __str__(self):\n        return 'value=' + str(self.x)\nf = Foo(42)\nprint(f'{f!s}')" "value=42\n"

-- f-string default conversion tries __str__ for instances
#eval assertPy "class Foo:\n    def __init__(self, x):\n        self.x = x\n    def __str__(self):\n        return 'Foo:' + str(self.x)\nf = Foo(7)\nprint(f'{f}')" "Foo:7\n"

-- f-string !r on int subclass with custom __repr__ (leanSpec pattern)
#eval assertPy "class BaseUint(int):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    def __repr__(self):\n        return type(self).__name__ + '(' + str(int(self)) + ')'\nclass Uint64(BaseUint):\n    pass\nx = Uint64(42)\nprint(f'{x!r}')" "Uint64(42)\n"

-- f-string !r in list comprehension (SSZModel __repr__ pattern)
#eval assertPy "class Point:\n    def __init__(self, x, y):\n        self.x = x\n        self.y = y\nfields = ['x', 'y']\np = Point(10, 20)\nresult = ' '.join([f'{name}={getattr(p, name)!r}' for name in fields])\nprint(result)" "x=10 y=20\n"

-- f-string !r with string field values shows quotes
#eval assertPy "class Cfg:\n    def __init__(self, name):\n        self.name = name\nc = Cfg('alice')\nprint(f'name={c.name!r}')" "name='alice'\n"

-- ============================================================
-- Phase 9a: Comprehensive end-to-end leanSpec type tests
-- ============================================================

-- Test A: Full SSZType ABC + BaseUint + Uint64 end-to-end
-- Mirrors: leanSpec exceptions.py + ssz_base.py + uint.py
#eval assertPy "import io\nfrom abc import ABC, abstractmethod\nfrom typing import override\nclass SSZError(Exception):\n    pass\nclass SSZTypeError(SSZError):\n    pass\nclass SSZValueError(SSZError):\n    pass\nclass SSZSerializationError(SSZError):\n    pass\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n    @classmethod\n    @abstractmethod\n    def get_byte_length(cls):\n        pass\n    @abstractmethod\n    def serialize(self, stream):\n        pass\n    @classmethod\n    @abstractmethod\n    def deserialize(cls, stream, scope):\n        pass\n    def encode_bytes(self):\n        with io.BytesIO() as stream:\n            self.serialize(stream)\n            return stream.getvalue()\n    @classmethod\n    def decode_bytes(cls, data):\n        with io.BytesIO(data) as stream:\n            return cls.deserialize(stream, len(data))\nclass BaseUint(int, SSZType):\n    __slots__ = ()\n    BITS = 64\n    def __new__(cls, value):\n        if not isinstance(value, int) or isinstance(value, bool):\n            raise SSZTypeError(f'Expected int, got {type(value).__name__}')\n        int_value = int(value)\n        max_value = 2 ** cls.BITS - 1\n        if not (0 <= int_value <= max_value):\n            raise SSZValueError(f'{int_value} out of range for {cls.__name__}')\n        return super().__new__(cls, int_value)\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    @override\n    def get_byte_length(cls):\n        return cls.BITS // 8\n    @override\n    def serialize(self, stream):\n        data = self.encode_bytes()\n        stream.write(data)\n        return len(data)\n    @classmethod\n    @override\n    def deserialize(cls, stream, scope):\n        byte_length = cls.get_byte_length()\n        if scope != byte_length:\n            raise SSZSerializationError(f'invalid scope')\n        data = stream.read(byte_length)\n        if len(data) != byte_length:\n            raise SSZSerializationError(f'short read')\n        return cls.decode_bytes(data)\n    @override\n    def encode_bytes(self):\n        return int(self).to_bytes(self.get_byte_length(), 'little')\n    @classmethod\n    @override\n    def decode_bytes(cls, data):\n        expected = cls.get_byte_length()\n        if len(data) != expected:\n            raise SSZSerializationError(f'expected {expected} bytes')\n        return cls(int.from_bytes(data, 'little'))\n    def __add__(self, other):\n        if not isinstance(other, BaseUint):\n            raise TypeError(f'bad type for +')\n        return type(self)(int(self) + int(other))\n    def __sub__(self, other):\n        if not isinstance(other, BaseUint):\n            raise TypeError(f'bad type for -')\n        return type(self)(int(self) - int(other))\n    def __eq__(self, other):\n        if not isinstance(other, BaseUint):\n            raise TypeError(f'bad type for ==')\n        return int(self) == int(other)\n    def __lt__(self, other):\n        if not isinstance(other, BaseUint):\n            raise TypeError(f'bad type for <')\n        return int(self) < int(other)\n    def __repr__(self):\n        return f'{type(self).__name__}({int(self)})'\n    def __str__(self):\n        return str(int(self))\n    def __hash__(self):\n        return hash((type(self).__name__, int(self)))\nclass Uint8(BaseUint):\n    BITS = 8\nclass Uint64(BaseUint):\n    BITS = 64\n# Test construction\na = Uint64(100)\nb = Uint64(3)\nprint(repr(a))\nprint(str(a))\n# Test arithmetic\nresult = a + b\nprint(repr(result))\n# Test validation\ntry:\n    Uint8(256)\nexcept SSZValueError as e:\n    print('overflow caught')\ntry:\n    Uint64(True)\nexcept SSZTypeError as e:\n    print('bool rejected')\n# Test comparison\nprint(a == Uint64(100))\nprint(a < Uint64(200))\n# Test hash\nprint(hash(a) == hash(Uint64(100)))\n# Test encode/decode round-trip\nencoded = a.encode_bytes()\nprint(len(encoded))\ndecoded = Uint64.decode_bytes(encoded)\nprint(repr(decoded))\nprint(a == decoded)\n# Test stream serialize/deserialize\nstream = io.BytesIO()\na.serialize(stream)\nstream.seek(0)\ndeserialized = Uint64.deserialize(stream, 8)\nprint(repr(deserialized))\n# Test is_fixed_size / get_byte_length\nprint(Uint64.is_fixed_size())\nprint(Uint64.get_byte_length())\nprint(Uint8.get_byte_length())\n# Test sorted\nvals = [Uint64(5), Uint64(1), Uint64(3)]\nprint([repr(x) for x in sorted(vals)])" "Uint64(100)\n100\nUint64(103)\noverflow caught\nbool rejected\nTrue\nTrue\nTrue\n8\nUint64(100)\nTrue\nUint64(100)\nTrue\n8\n1\n['Uint64(1)', 'Uint64(3)', 'Uint64(5)']\n"

-- Test B: Full BaseBytes + Bytes32 end-to-end
-- Mirrors: leanSpec byte_arrays.py
#eval assertPy "import io\nfrom abc import ABC, abstractmethod\nfrom typing import override\nclass SSZError(Exception):\n    pass\nclass SSZValueError(SSZError):\n    pass\nclass SSZSerializationError(SSZError):\n    pass\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n    @classmethod\n    @abstractmethod\n    def get_byte_length(cls):\n        pass\n    @abstractmethod\n    def serialize(self, stream):\n        pass\n    @classmethod\n    @abstractmethod\n    def deserialize(cls, stream, scope):\n        pass\n    def encode_bytes(self):\n        with io.BytesIO() as stream:\n            self.serialize(stream)\n            return stream.getvalue()\n    @classmethod\n    def decode_bytes(cls, data):\n        with io.BytesIO(data) as stream:\n            return cls.deserialize(stream, len(data))\nclass BaseBytes(bytes, SSZType):\n    __slots__ = ()\n    LENGTH = 0\n    @staticmethod\n    def _coerce_to_bytes(value):\n        if isinstance(value, (bytes, bytearray)):\n            return bytes(value)\n        if isinstance(value, str):\n            return bytes.fromhex(value.removeprefix('0x'))\n        raise TypeError(f'Cannot coerce {type(value).__name__} to bytes')\n    def __new__(cls, value=b''):\n        if not hasattr(cls, 'LENGTH'):\n            raise TypeError(f'{cls.__name__} must define LENGTH')\n        b = cls._coerce_to_bytes(value)\n        if len(b) != cls.LENGTH:\n            raise SSZValueError(f'{cls.__name__} requires {cls.LENGTH} bytes, got {len(b)}')\n        return super().__new__(cls, b)\n    @classmethod\n    def zero(cls):\n        return cls(b'\\x00' * cls.LENGTH)\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    @override\n    def get_byte_length(cls):\n        return cls.LENGTH\n    @override\n    def serialize(self, stream):\n        stream.write(self)\n        return len(self)\n    @classmethod\n    @override\n    def deserialize(cls, stream, scope):\n        if scope != cls.LENGTH:\n            raise SSZSerializationError(f'expected {cls.LENGTH} bytes, got {scope}')\n        data = stream.read(scope)\n        if len(data) != scope:\n            raise SSZSerializationError(f'short read')\n        return cls(data)\n    @override\n    def encode_bytes(self):\n        return bytes(self)\n    @classmethod\n    @override\n    def decode_bytes(cls, data):\n        if len(data) != cls.LENGTH:\n            raise SSZValueError(f'wrong length')\n        return cls(data)\n    def hex(self):\n        return bytes(self).hex()\n    def __repr__(self):\n        return f'{type(self).__name__}({self.hex()})'\n    def __hash__(self):\n        return hash((type(self).__name__, bytes(self)))\nclass Bytes4(BaseBytes):\n    LENGTH = 4\nclass Bytes32(BaseBytes):\n    LENGTH = 32\n# Construction from bytes\nx = Bytes4(b'\\xde\\xad\\xbe\\xef')\nprint(repr(x))\nprint(x.hex())\n# Construction from hex string\ny = Bytes4('deadbeef')\nprint(repr(y))\n# Construction from 0x-prefixed hex\nz = Bytes4('0xdeadbeef')\nprint(repr(z))\n# Zero construction\nw = Bytes32.zero()\nprint(len(w))\nprint(w[0])\n# Length validation\ntry:\n    Bytes4(b'\\x01\\x02')\nexcept SSZValueError as e:\n    print('length rejected')\n# Type checks\nprint(Bytes4.is_fixed_size())\nprint(Bytes4.get_byte_length())\nprint(Bytes32.get_byte_length())\n# Encode/decode round-trip\nencoded = x.encode_bytes()\ndecoded = Bytes4.decode_bytes(encoded)\nprint(repr(decoded))\n# Stream serialize/deserialize\nstream = io.BytesIO()\nx.serialize(stream)\nstream.seek(0)\ndeserialized = Bytes4.deserialize(stream, 4)\nprint(repr(deserialized))\n# Hash\nprint(hash(x) == hash(Bytes4(b'\\xde\\xad\\xbe\\xef')))" "Bytes4(deadbeef)\ndeadbeef\nBytes4(deadbeef)\nBytes4(deadbeef)\n32\n0\nlength rejected\nTrue\n4\n32\nBytes4(deadbeef)\nBytes4(deadbeef)\nTrue\n"

-- Test C-pre: isinstance(classObj, type) returns True
#eval assertPy "class Foo:\n    pass\nprint(isinstance(Foo, type))\nprint(isinstance(int, type))\nprint(isinstance(42, type))" "True\nTrue\nFalse\n"

-- Test C-pre2: ClassVar[int] and Generic[T] subscripting on typing stubs
#eval assertPy "from typing import ClassVar, Generic, TypeVar\nT = TypeVar('T')\nresult = ClassVar[int]\nprint(result is None or result == ClassVar)\n# Generic subscript\nresult2 = Generic[T]\nprint(True)" "True\nTrue\n"

-- Test C: Boolean type with SSZType interface
-- Mirrors: leanSpec boolean.py
#eval assertPy "from abc import ABC, abstractmethod\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n    @classmethod\n    @abstractmethod\n    def get_byte_length(cls):\n        pass\nclass Boolean(int, SSZType):\n    __slots__ = ()\n    def __new__(cls, value):\n        if not isinstance(value, int):\n            raise TypeError(f'Expected bool or int, got {type(value).__name__}')\n        if value not in (0, 1, True, False):\n            raise ValueError(f'Boolean must be 0 or 1, got {value}')\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return 1\n    def __add__(self, other):\n        raise TypeError('Arithmetic not supported for Boolean')\n    def __sub__(self, other):\n        raise TypeError('Arithmetic not supported for Boolean')\n    def __and__(self, other):\n        if not isinstance(other, Boolean):\n            raise TypeError('Expected Boolean operand')\n        return Boolean(int(self) & int(other))\n    def __or__(self, other):\n        if not isinstance(other, Boolean):\n            raise TypeError('Expected Boolean operand')\n        return Boolean(int(self) | int(other))\n    def __xor__(self, other):\n        if not isinstance(other, Boolean):\n            raise TypeError('Expected Boolean operand')\n        return Boolean(int(self) ^ int(other))\n    def __eq__(self, other):\n        return isinstance(other, int) and int(self) == int(other)\n    def __repr__(self):\n        return f'Boolean({int(self)})'\n    def __hash__(self):\n        return hash(('Boolean', int(self)))\n# Construction\nbt = Boolean(True)\nbf = Boolean(False)\nprint(repr(bt))\nprint(repr(bf))\n# Values\nprint(int(bt))\nprint(int(bf))\n# Equality with native types\nprint(bt == 1)\nprint(bf == 0)\nprint(bt == True)\nprint(bf == False)\n# Arithmetic rejection\ntry:\n    bt + bf\nexcept TypeError as e:\n    print('add rejected')\n# Bitwise operators work\nprint(repr(bt & bf))\nprint(repr(bt | bf))\nprint(repr(bt ^ bf))\n# Value validation\ntry:\n    Boolean(2)\nexcept ValueError as e:\n    print('value 2 rejected')\n# Type checks\nprint(Boolean.is_fixed_size())\nprint(Boolean.get_byte_length())\n# Hash\nprint(hash(bt) == hash(Boolean(1)))" "Boolean(1)\nBoolean(0)\n1\n0\nTrue\nTrue\nTrue\nTrue\nadd rejected\nBoolean(0)\nBoolean(1)\nBoolean(1)\nvalue 2 rejected\nTrue\n1\nTrue\n"

-- ============================================================
-- Phase 9a continued: model_fields annotation access
-- ============================================================

-- model_fields stores actual type annotations (not just "Any")
#eval assertPy "from pydantic import BaseModel, ConfigDict\nclass MyInt:\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return 8\nclass MyModel(BaseModel):\n    model_config = ConfigDict(frozen=True)\n    x: MyInt\n    y: MyInt\nfor name, field in MyModel.model_fields.items():\n    ann = field.annotation\n    print(f'{name}: fixed={ann.is_fixed_size()} bytes={ann.get_byte_length()}')" "x: fixed=True bytes=8\ny: fixed=True bytes=8\n"

-- model_fields.values() iteration with annotation access
#eval assertPy "from pydantic import BaseModel, ConfigDict\nclass FieldType:\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return 4\nclass M(BaseModel):\n    model_config = ConfigDict(frozen=True)\n    a: FieldType\n    b: FieldType\n    c: FieldType\ntotal = sum(f.annotation.get_byte_length() for f in M.model_fields.values())\nprint(total)\nall_fixed = all(f.annotation.is_fixed_size() for f in M.model_fields.values())\nprint(all_fixed)" "12\nTrue\n"

-- isinstance(annotation, type) for _get_ssz_field_type pattern
#eval assertPy "from pydantic import BaseModel, ConfigDict\nclass SSZType:\n    pass\nclass Uint64(int, SSZType):\n    BITS = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\nclass M(BaseModel):\n    model_config = ConfigDict(frozen=True)\n    slot: Uint64\nfor field in M.model_fields.values():\n    ann = field.annotation\n    print(isinstance(ann, type))\n    print(issubclass(ann, SSZType))" "True\nTrue\n"

-- ============================================================
-- Phase 9a: Container end-to-end (SSZ Container pattern)
-- ============================================================

-- Test D: Full Container with typed fields, serialize/deserialize
-- Mirrors: leanSpec container.py + ssz_base.py + base.py
#eval assertPy "import io\nfrom abc import ABC, abstractmethod\nfrom typing import override\nfrom pydantic import BaseModel, ConfigDict\nclass SSZError(Exception):\n    pass\nclass SSZTypeError(SSZError):\n    pass\nclass SSZSerializationError(SSZError):\n    pass\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n    @classmethod\n    @abstractmethod\n    def get_byte_length(cls):\n        pass\n    @abstractmethod\n    def serialize(self, stream):\n        pass\n    @classmethod\n    @abstractmethod\n    def deserialize(cls, stream, scope):\n        pass\n    def encode_bytes(self):\n        with io.BytesIO() as stream:\n            self.serialize(stream)\n            return stream.getvalue()\n    @classmethod\n    def decode_bytes(cls, data):\n        with io.BytesIO(data) as stream:\n            return cls.deserialize(stream, len(data))\nclass CamelModel(BaseModel):\n    model_config = ConfigDict(validate_default=True, arbitrary_types_allowed=True)\nclass StrictBaseModel(CamelModel):\n    model_config = ConfigDict(extra='forbid', frozen=True, strict=True)\nclass SSZModel(StrictBaseModel, SSZType):\n    def __len__(self):\n        return len(type(self).model_fields)\nclass BaseUint(int, SSZType):\n    __slots__ = ()\n    BITS = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    @override\n    def get_byte_length(cls):\n        return cls.BITS // 8\n    @override\n    def serialize(self, stream):\n        data = self.encode_bytes()\n        stream.write(data)\n        return len(data)\n    @classmethod\n    @override\n    def deserialize(cls, stream, scope):\n        data = stream.read(scope)\n        return cls.decode_bytes(data)\n    @override\n    def encode_bytes(self):\n        return int(self).to_bytes(self.get_byte_length(), 'little')\n    @classmethod\n    @override\n    def decode_bytes(cls, data):\n        return cls(int.from_bytes(data, 'little'))\n    def __repr__(self):\n        return f'{type(self).__name__}({int(self)})'\nclass Uint64(BaseUint):\n    BITS = 64\nclass Uint8(BaseUint):\n    BITS = 8\nclass BaseBytes(bytes, SSZType):\n    __slots__ = ()\n    LENGTH = 0\n    def __new__(cls, value=b''):\n        if isinstance(value, (bytes, bytearray)):\n            b = bytes(value)\n        elif isinstance(value, str):\n            b = bytes.fromhex(value)\n        else:\n            raise TypeError('Cannot coerce')\n        if len(b) != cls.LENGTH:\n            raise SSZError(f'wrong length')\n        return super().__new__(cls, b)\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    @override\n    def get_byte_length(cls):\n        return cls.LENGTH\n    @override\n    def serialize(self, stream):\n        stream.write(self)\n        return len(self)\n    @classmethod\n    @override\n    def deserialize(cls, stream, scope):\n        data = stream.read(scope)\n        return cls(data)\n    @override\n    def encode_bytes(self):\n        return bytes(self)\n    @classmethod\n    @override\n    def decode_bytes(cls, data):\n        return cls(data)\n    def hex(self):\n        return bytes(self).hex()\n    def __repr__(self):\n        return f'{type(self).__name__}({self.hex()})'\nclass Bytes4(BaseBytes):\n    LENGTH = 4\nclass Bytes32(BaseBytes):\n    LENGTH = 32\nOFFSET_BYTE_LENGTH = 4\nclass Container(SSZModel):\n    @staticmethod\n    def _get_ssz_field_type(annotation):\n        if not (isinstance(annotation, type) and issubclass(annotation, SSZType)):\n            raise SSZTypeError(f'Expected SSZType subclass, got {annotation}')\n        return annotation\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return all(\n            cls._get_ssz_field_type(field.annotation).is_fixed_size()\n            for field in cls.model_fields.values()\n        )\n    @classmethod\n    @override\n    def get_byte_length(cls):\n        if not cls.is_fixed_size():\n            raise SSZTypeError('variable-size container')\n        return sum(\n            cls._get_ssz_field_type(field.annotation).get_byte_length()\n            for field in cls.model_fields.values()\n        )\n    @override\n    def serialize(self, stream):\n        fixed_parts = []\n        variable_data = []\n        for field_name in type(self).model_fields:\n            value = getattr(self, field_name)\n            field_type = type(value)\n            if field_type.is_fixed_size():\n                fixed_parts.append(value.encode_bytes())\n            else:\n                fixed_parts.append(b'')\n                variable_data.append(value.encode_bytes())\n        offset = sum(len(part) if part else OFFSET_BYTE_LENGTH for part in fixed_parts)\n        var_iter = iter(variable_data)\n        for part in fixed_parts:\n            if part:\n                stream.write(part)\n            else:\n                stream.write(Uint32(offset).encode_bytes())\n                offset += len(next(var_iter))\n        for data in variable_data:\n            stream.write(data)\n        return offset\n    @classmethod\n    @override\n    def deserialize(cls, stream, scope):\n        fields = {}\n        for field_name, field_info in cls.model_fields.items():\n            field_type = cls._get_ssz_field_type(field_info.annotation)\n            if field_type.is_fixed_size():\n                size = field_type.get_byte_length()\n                data = stream.read(size)\n                fields[field_name] = field_type.decode_bytes(data)\n        return cls(**fields)\nclass Uint32(BaseUint):\n    BITS = 32\n# Define a simple container\nclass Checkpoint(Container):\n    epoch: Uint64\n    root: Bytes32\n# Test construction\nroot_bytes = Bytes32(b'\\x00' * 32)\ncp = Checkpoint(epoch=Uint64(42), root=root_bytes)\nprint(cp.epoch)\nprint(type(cp.epoch).__name__)\nprint(len(cp.root))\n# Test is_fixed_size\nprint(Checkpoint.is_fixed_size())\n# Test get_byte_length: 8 (Uint64) + 32 (Bytes32) = 40\nprint(Checkpoint.get_byte_length())\n# Test len\nprint(len(cp))\n# Test serialize/deserialize round-trip\nstream = io.BytesIO()\ncp.serialize(stream)\nraw = stream.getvalue()\nprint(len(raw))\nstream2 = io.BytesIO(raw)\ncp2 = Checkpoint.deserialize(stream2, len(raw))\nprint(cp2.epoch)\nprint(bytes(cp2.root).hex())" "42\nUint64\n32\nTrue\n40\n2\n40\n42\n0000000000000000000000000000000000000000000000000000000000000000\n"

-- ============================================================
-- Phase 9a: SSZVector with explicit ELEMENT_TYPE
-- ============================================================

-- Test E: SSZVector with fixed-size elements, serialize/deserialize
#eval assertPy "import io\nfrom abc import ABC, abstractmethod\nfrom typing import ClassVar, override\nfrom pydantic import BaseModel, ConfigDict\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n    @classmethod\n    @abstractmethod\n    def get_byte_length(cls):\n        pass\n    @abstractmethod\n    def serialize(self, stream):\n        pass\n    @classmethod\n    @abstractmethod\n    def deserialize(cls, stream, scope):\n        pass\n    def encode_bytes(self):\n        with io.BytesIO() as stream:\n            self.serialize(stream)\n            return stream.getvalue()\n    @classmethod\n    def decode_bytes(cls, data):\n        with io.BytesIO(data) as stream:\n            return cls.deserialize(stream, len(data))\nclass StrictBaseModel(BaseModel):\n    model_config = ConfigDict(extra='forbid', frozen=True, strict=True)\nclass SSZModel(StrictBaseModel, SSZType):\n    pass\nclass BaseUint(int, SSZType):\n    __slots__ = ()\n    BITS = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    @override\n    def get_byte_length(cls):\n        return cls.BITS // 8\n    @override\n    def serialize(self, stream):\n        data = self.encode_bytes()\n        stream.write(data)\n        return len(data)\n    @classmethod\n    @override\n    def deserialize(cls, stream, scope):\n        data = stream.read(scope)\n        return cls.decode_bytes(data)\n    @override\n    def encode_bytes(self):\n        return int(self).to_bytes(self.get_byte_length(), 'little')\n    @classmethod\n    @override\n    def decode_bytes(cls, data):\n        return cls(int.from_bytes(data, 'little'))\n    def __repr__(self):\n        return f'{type(self).__name__}({int(self)})'\nclass Uint64(BaseUint):\n    BITS = 64\nclass SSZVector(SSZModel):\n    ELEMENT_TYPE: ClassVar = None\n    LENGTH: ClassVar = 0\n    data: tuple = ()\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return cls.ELEMENT_TYPE.is_fixed_size()\n    @classmethod\n    @override\n    def get_byte_length(cls):\n        return cls.ELEMENT_TYPE.get_byte_length() * cls.LENGTH\n    @override\n    def serialize(self, stream):\n        total = 0\n        for element in self.data:\n            total += element.serialize(stream)\n        return total\n    @classmethod\n    @override\n    def deserialize(cls, stream, scope):\n        elem_size = cls.ELEMENT_TYPE.get_byte_length()\n        elements = []\n        for i in range(cls.LENGTH):\n            data = stream.read(elem_size)\n            elements.append(cls.ELEMENT_TYPE.decode_bytes(data))\n        return cls(data=tuple(elements))\n    def __len__(self):\n        return len(self.data)\n    def __getitem__(self, index):\n        return self.data[index]\nclass Slots(SSZVector):\n    ELEMENT_TYPE = Uint64\n    LENGTH = 4\n# Test construction\nv = Slots(data=(Uint64(10), Uint64(20), Uint64(30), Uint64(40)))\nprint(len(v))\nprint(repr(v[0]))\nprint(repr(v[3]))\n# Test type properties\nprint(Slots.is_fixed_size())\nprint(Slots.get_byte_length())\n# Test serialize/deserialize round-trip\nstream = io.BytesIO()\nv.serialize(stream)\nraw = stream.getvalue()\nprint(len(raw))\nstream2 = io.BytesIO(raw)\nv2 = Slots.deserialize(stream2, len(raw))\nprint(len(v2))\nprint(repr(v2[0]))\nprint(repr(v2[3]))" "4\nUint64(10)\nUint64(40)\nTrue\n32\n32\n4\nUint64(10)\nUint64(40)\n"

-- ============================================================
-- Phase 9a: SSZList with explicit ELEMENT_TYPE
-- ============================================================

-- Test F: SSZList with variable length
#eval assertPy "import io\nfrom abc import ABC, abstractmethod\nfrom typing import ClassVar, override\nfrom pydantic import BaseModel, ConfigDict\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n    @classmethod\n    @abstractmethod\n    def get_byte_length(cls):\n        pass\n    def encode_bytes(self):\n        with io.BytesIO() as stream:\n            self.serialize(stream)\n            return stream.getvalue()\n    @classmethod\n    def decode_bytes(cls, data):\n        with io.BytesIO(data) as stream:\n            return cls.deserialize(stream, len(data))\nclass StrictBaseModel(BaseModel):\n    model_config = ConfigDict(extra='forbid', frozen=True, strict=True)\nclass SSZModel(StrictBaseModel, SSZType):\n    pass\nclass BaseUint(int, SSZType):\n    __slots__ = ()\n    BITS = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    @override\n    def get_byte_length(cls):\n        return cls.BITS // 8\n    def serialize(self, stream):\n        data = self.encode_bytes()\n        stream.write(data)\n        return len(data)\n    @classmethod\n    def deserialize(cls, stream, scope):\n        data = stream.read(scope)\n        return cls.decode_bytes(data)\n    def encode_bytes(self):\n        return int(self).to_bytes(self.get_byte_length(), 'little')\n    @classmethod\n    def decode_bytes(cls, data):\n        return cls(int.from_bytes(data, 'little'))\n    def __repr__(self):\n        return f'{type(self).__name__}({int(self)})'\nclass Uint64(BaseUint):\n    BITS = 64\nclass SSZList(SSZModel):\n    ELEMENT_TYPE: ClassVar = None\n    LIMIT: ClassVar = 0\n    data: tuple = ()\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return False\n    @classmethod\n    @override\n    def get_byte_length(cls):\n        raise TypeError('SSZList is variable-size')\n    def serialize(self, stream):\n        total = 0\n        for element in self.data:\n            total += element.serialize(stream)\n        return total\n    @classmethod\n    def deserialize(cls, stream, scope):\n        elem_size = cls.ELEMENT_TYPE.get_byte_length()\n        count = scope // elem_size\n        elements = []\n        for i in range(count):\n            data = stream.read(elem_size)\n            elements.append(cls.ELEMENT_TYPE.decode_bytes(data))\n        return cls(data=tuple(elements))\n    def __len__(self):\n        return len(self.data)\n    def __getitem__(self, index):\n        return self.data[index]\n    def __iter__(self):\n        return iter(self.data)\nclass SlotList(SSZList):\n    ELEMENT_TYPE = Uint64\n    LIMIT = 1024\n# Test construction\nsl = SlotList(data=(Uint64(1), Uint64(2), Uint64(3)))\nprint(len(sl))\nprint(repr(sl[0]))\nprint(repr(sl[2]))\n# Test is_fixed_size (SSZList is always variable)\nprint(SlotList.is_fixed_size())\n# Test serialize/deserialize round-trip\nstream = io.BytesIO()\nsl.serialize(stream)\nraw = stream.getvalue()\nprint(len(raw))\nstream2 = io.BytesIO(raw)\nsl2 = SlotList.deserialize(stream2, len(raw))\nprint(len(sl2))\nprint([repr(x) for x in sl2])" "3\nUint64(1)\nUint64(3)\nFalse\n24\n3\n['Uint64(1)', 'Uint64(2)', 'Uint64(3)']\n"

-- ============================================================
-- Phase 9a: BaseBitvector bitfield pattern
-- ============================================================

-- Test G: BaseBitvector with Boolean elements and decode round-trip
#eval assertPy "from abc import ABC, abstractmethod\nfrom typing import ClassVar, override\nfrom pydantic import BaseModel, ConfigDict\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n    @classmethod\n    @abstractmethod\n    def get_byte_length(cls):\n        pass\nclass StrictBaseModel(BaseModel):\n    model_config = ConfigDict(extra='forbid', frozen=True, strict=True)\nclass SSZModel(StrictBaseModel, SSZType):\n    pass\nclass Boolean(int, SSZType):\n    def __new__(cls, value):\n        if value not in (0, 1, True, False):\n            raise ValueError(f'Boolean must be 0 or 1')\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return 1\n    def __repr__(self):\n        return f'Boolean({int(self)})'\nclass BaseBitvector(SSZModel):\n    LENGTH: ClassVar = 0\n    data: tuple = ()\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    @override\n    def get_byte_length(cls):\n        return (cls.LENGTH + 7) // 8\n    def __len__(self):\n        return len(self.data)\n    def __getitem__(self, index):\n        return self.data[index]\n    def encode_bytes(self):\n        byte_length = self.get_byte_length()\n        result = [0] * byte_length\n        for i, bit in enumerate(self.data):\n            if int(bit):\n                result[i // 8] = result[i // 8] | (1 << (i % 8))\n        return bytes(result)\n    @classmethod\n    def decode_bytes(cls, data):\n        expected = cls.get_byte_length()\n        if len(data) != expected:\n            raise ValueError(f'expected {expected} bytes, got {len(data)}')\n        bits = tuple(Boolean((data[i // 8] >> (i % 8)) & 1) for i in range(cls.LENGTH))\n        return cls(data=bits)\nclass JustBits(BaseBitvector):\n    LENGTH = 8\n# Test construction\nbv = JustBits(data=(Boolean(1), Boolean(0), Boolean(1), Boolean(0), Boolean(0), Boolean(0), Boolean(0), Boolean(1)))\nprint(len(bv))\nprint(repr(bv[0]))\nprint(repr(bv[7]))\n# Test type properties\nprint(JustBits.is_fixed_size())\nprint(JustBits.get_byte_length())\n# Test encode_bytes: bits 0,2,7 set = 0b10000101 = 0x85\nencoded = bv.encode_bytes()\nprint(len(encoded))\nprint(hex(encoded[0]))\n# Test decode round-trip\nbv2 = JustBits.decode_bytes(encoded)\nprint(len(bv2))\nprint(int(bv2[0]))\nprint(int(bv2[2]))\nprint(int(bv2[7]))\nprint(int(bv2[1]))" "8\nBoolean(1)\nBoolean(1)\nTrue\n1\n1\n0x85\n8\n1\n1\n1\n0\n"

-- ============================================================
-- Phase 9a: BaseBitlist variable-length bitfield
-- ============================================================

-- Test H: BaseBitlist with delimiter bit encoding/decoding
#eval assertPy "from abc import ABC, abstractmethod\nfrom typing import ClassVar, override\nfrom pydantic import BaseModel, ConfigDict\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n    @classmethod\n    @abstractmethod\n    def get_byte_length(cls):\n        pass\nclass StrictBaseModel(BaseModel):\n    model_config = ConfigDict(extra='forbid', frozen=True, strict=True)\nclass SSZModel(StrictBaseModel, SSZType):\n    pass\nclass Boolean(int, SSZType):\n    def __new__(cls, value):\n        if value not in (0, 1, True, False):\n            raise ValueError(f'Boolean must be 0 or 1')\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return 1\n    def __repr__(self):\n        return f'Boolean({int(self)})'\nclass BaseBitlist(SSZModel):\n    LIMIT: ClassVar = 0\n    data: tuple = ()\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return False\n    @classmethod\n    @override\n    def get_byte_length(cls):\n        raise TypeError('BaseBitlist is variable-size')\n    def __len__(self):\n        return len(self.data)\n    def __getitem__(self, index):\n        return self.data[index]\n    def encode_bytes(self):\n        num_bits = len(self.data)\n        if num_bits == 0:\n            return b'\\x01'\n        byte_len = (num_bits + 7) // 8\n        byte_array = [0] * byte_len\n        for i, bit in enumerate(self.data):\n            if int(bit):\n                byte_array[i // 8] = byte_array[i // 8] | (1 << (i % 8))\n        if num_bits % 8 == 0:\n            return bytes(byte_array) + b'\\x01'\n        else:\n            byte_array[num_bits // 8] = byte_array[num_bits // 8] | (1 << (num_bits % 8))\n            return bytes(byte_array)\n    @classmethod\n    def decode_bytes(cls, data):\n        if len(data) == 0:\n            raise ValueError('cannot decode empty bytes')\n        delimiter_pos = None\n        byte_idx = len(data) - 1\n        while byte_idx >= 0:\n            byte_val = data[byte_idx]\n            if byte_val != 0:\n                bit_idx = byte_val.bit_length() - 1\n                delimiter_pos = byte_idx * 8 + bit_idx\n                break\n            byte_idx = byte_idx - 1\n        if delimiter_pos is None:\n            raise ValueError('no delimiter bit found')\n        num_bits = delimiter_pos\n        bits = tuple(Boolean((data[i // 8] >> (i % 8)) & 1) for i in range(num_bits))\n        return cls(data=bits)\n\nclass MyBitlist(BaseBitlist):\n    LIMIT = 16\n\n# Test 1: 5 bits [1,0,1,1,0] -> delimiter at bit 5\nbl = MyBitlist(data=(Boolean(1), Boolean(0), Boolean(1), Boolean(0), Boolean(1)))\nprint(len(bl))\nprint(BaseBitlist.is_fixed_size())\nprint(int(bl[0]))\nprint(int(bl[4]))\n# Encode: bits 0,2,4 set + delimiter at bit 5\n# byte = bit0=1,bit1=0,bit2=1,bit3=0,bit4=1,bit5(delim)=1 = 0b00110101 = 0x35\nencoded = bl.encode_bytes()\nprint(len(encoded))\nprint(hex(encoded[0]))\n# Decode round-trip\nbl2 = MyBitlist.decode_bytes(encoded)\nprint(len(bl2))\nprint(int(bl2[0]))\nprint(int(bl2[2]))\nprint(int(bl2[4]))\nprint(int(bl2[1]))\n\n# Test 2: empty bitlist -> b'\\x01'\nempty = MyBitlist(data=())\nencoded_empty = empty.encode_bytes()\nprint(len(encoded_empty))\nprint(encoded_empty[0])\n# Decode empty round-trip\nempty2 = MyBitlist.decode_bytes(encoded_empty)\nprint(len(empty2))\n\n# Test 3: 8 bits -> delimiter in extra byte\nbits8 = MyBitlist(data=tuple(Boolean(1) for _ in range(8)))\nencoded8 = bits8.encode_bytes()\nprint(len(encoded8))\nprint(hex(encoded8[0]))\nprint(encoded8[1])\n# Decode round-trip\nbits8_rt = MyBitlist.decode_bytes(encoded8)\nprint(len(bits8_rt))\nprint(int(bits8_rt[0]))\nprint(int(bits8_rt[7]))" "5\nFalse\n1\n1\n1\n0x35\n5\n1\n1\n1\n0\n1\n1\n0\n2\n0xff\n1\n8\n1\n1\n"

-- ============================================================
-- Phase 9a: BaseByteList variable-length byte list
-- ============================================================

-- Test I: BaseByteList with data field and serialize/deserialize
#eval assertPy "import io\nfrom abc import ABC, abstractmethod\nfrom typing import ClassVar, override\nfrom pydantic import BaseModel, ConfigDict\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n    @classmethod\n    @abstractmethod\n    def get_byte_length(cls):\n        pass\n    def encode_bytes(self):\n        with io.BytesIO() as stream:\n            self.serialize(stream)\n            return stream.getvalue()\n    @classmethod\n    def decode_bytes(cls, data):\n        with io.BytesIO(data) as stream:\n            return cls.deserialize(stream, len(data))\nclass StrictBaseModel(BaseModel):\n    model_config = ConfigDict(extra='forbid', frozen=True, strict=True)\nclass SSZModel(StrictBaseModel, SSZType):\n    pass\nclass BaseByteList(SSZModel):\n    LIMIT: ClassVar = 0\n    data: bytes = b''\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return False\n    @classmethod\n    @override\n    def get_byte_length(cls):\n        raise TypeError('BaseByteList is variable-size')\n    @override\n    def serialize(self, stream):\n        stream.write(self.data)\n        return len(self.data)\n    @classmethod\n    @override\n    def deserialize(cls, stream, scope):\n        data = stream.read(scope)\n        if len(data) != scope:\n            raise ValueError(f'expected {scope} bytes')\n        return cls(data=data)\n    def __eq__(self, other):\n        if isinstance(other, type(self)):\n            return self.data == other.data\n        return False\n    def __hash__(self):\n        return hash((type(self).__name__, self.data))\n    def hex(self):\n        return self.data.hex()\n    def __repr__(self):\n        return f'{type(self).__name__}({self.data.hex()})'\n\nclass ByteListMiB(BaseByteList):\n    LIMIT = 1048576\n\n# Test construction\nbl = ByteListMiB(data=b'\\x01\\x02\\x03')\nprint(repr(bl))\nprint(bl.hex())\nprint(ByteListMiB.is_fixed_size())\n\n# Test serialize/deserialize round-trip\nstream = io.BytesIO()\nbl.serialize(stream)\nraw = stream.getvalue()\nprint(len(raw))\nprint(raw.hex())\n\nstream2 = io.BytesIO(raw)\nbl2 = ByteListMiB.deserialize(stream2, len(raw))\nprint(repr(bl2))\nprint(bl == bl2)\n\n# Test encode/decode bytes\nencoded = bl.encode_bytes()\nprint(encoded.hex())\nbl3 = ByteListMiB.decode_bytes(encoded)\nprint(bl == bl3)\n\n# Test empty\nempty = ByteListMiB(data=b'')\nprint(repr(empty))\nprint(len(empty.encode_bytes()))\n\n# Test hash\nprint(hash(bl) == hash(bl))" "ByteListMiB(LIMIT=0, data=b'\\x01\\x02\\x03')\n010203\nFalse\n3\n010203\nByteListMiB(LIMIT=0, data=b'\\x01\\x02\\x03')\nTrue\n010203\nTrue\nByteListMiB(LIMIT=0, data=b'')\n0\nTrue\n"

-- ============================================================
-- Phase 9b: SSZ Merkleization — singledispatch + hash_tree_root
-- ============================================================

-- Test J: functools.singledispatch basic dispatch
#eval assertPy "from functools import singledispatch\n@singledispatch\ndef process(value):\n    return 'default'\n@process.register\ndef _(value: int):\n    return 'int'\n@process.register\ndef _(value: str):\n    return 'str'\nprint(process(42))\nprint(process('hello'))\nprint(process(3.14))" "int\nstr\ndefault\n"

-- Test K: hash_tree_root for BaseUint via singledispatch + hashlib
#eval assertPy "import hashlib\nfrom functools import singledispatch\nfrom abc import ABC, abstractmethod\nfrom typing import ClassVar, override\n\nBYTES_PER_CHUNK = 32\n\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n    @classmethod\n    @abstractmethod\n    def get_byte_length(cls):\n        pass\n\nclass BaseBytes(bytes, SSZType):\n    __slots__ = ()\n    LENGTH: ClassVar = 0\n    def __new__(cls, value=b''):\n        if isinstance(value, (bytes, bytearray)):\n            b = bytes(value)\n        elif isinstance(value, str):\n            b = bytes.fromhex(value)\n        else:\n            b = bytes(value)\n        return super().__new__(cls, b)\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return cls.LENGTH\n    def encode_bytes(self):\n        return bytes(self)\n    def hex(self):\n        return bytes(self).hex()\n    @classmethod\n    def zero(cls):\n        return cls(b'\\x00' * cls.LENGTH)\n\nclass Bytes32(BaseBytes):\n    LENGTH = 32\n\nZERO_HASH = Bytes32.zero()\n\nclass BaseUint(int, SSZType):\n    __slots__ = ()\n    BITS: ClassVar = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return cls.BITS // 8\n    def encode_bytes(self):\n        return int(self).to_bytes(self.get_byte_length(), 'little')\n\nclass Uint64(BaseUint):\n    BITS = 64\n\ndef hash_nodes(a, b):\n    return Bytes32(hashlib.sha256(bytes(a) + bytes(b)).digest())\n\ndef get_power_of_two_ceil(x):\n    if x <= 1:\n        return 1\n    return 1 << (x - 1).bit_length()\n\ndef pack_bytes(data):\n    if len(data) == 0:\n        return []\n    pad_len = BYTES_PER_CHUNK - (len(data) % BYTES_PER_CHUNK)\n    if pad_len < BYTES_PER_CHUNK:\n        data = data + b'\\x00' * pad_len\n    chunks = []\n    i = 0\n    while i < len(data):\n        chunks.append(Bytes32(data[i:i+BYTES_PER_CHUNK]))\n        i = i + BYTES_PER_CHUNK\n    return chunks\n\ndef merkleize(chunks, limit=None):\n    n = len(chunks)\n    if n == 0:\n        if limit is not None:\n            w = get_power_of_two_ceil(limit)\n            depth = (w - 1).bit_length() if w > 1 else 0\n            h = ZERO_HASH\n            for _ in range(depth):\n                h = hash_nodes(h, h)\n            return h\n        return ZERO_HASH\n    if limit is None:\n        width = get_power_of_two_ceil(n)\n    else:\n        width = get_power_of_two_ceil(limit)\n    if width == 1:\n        return chunks[0]\n    level = list(chunks)\n    subtree_size = 1\n    while subtree_size < width:\n        next_level = []\n        i = 0\n        while i < len(level):\n            left = level[i]\n            i = i + 1\n            if i < len(level):\n                right = level[i]\n                i = i + 1\n            else:\n                depth = (subtree_size - 1).bit_length() if subtree_size > 1 else 0\n                h = ZERO_HASH\n                for _ in range(depth):\n                    h = hash_nodes(h, h)\n                right = h\n            next_level.append(hash_nodes(left, right))\n        level = next_level\n        subtree_size = subtree_size * 2\n    return level[0]\n\ndef mix_in_length(root, length):\n    return hash_nodes(root, Bytes32(length.to_bytes(32, 'little')))\n\n@singledispatch\ndef hash_tree_root(value):\n    raise TypeError(f'unsupported type: {type(value).__name__}')\n\n@hash_tree_root.register\ndef _htr_uint(value: BaseUint):\n    return merkleize(pack_bytes(value.encode_bytes()))\n\n# Test hash_tree_root(Uint64(0)) = ZERO_HASH\nr0 = hash_tree_root(Uint64(0))\nprint(r0.hex())\n\n# Test hash_tree_root(Uint64(1))\nr1 = hash_tree_root(Uint64(1))\nprint(r1.hex())\n\n# Test get_power_of_two_ceil\nprint(get_power_of_two_ceil(0))\nprint(get_power_of_two_ceil(1))\nprint(get_power_of_two_ceil(3))\nprint(get_power_of_two_ceil(5))\n\n# Test merkleize with 2 chunks = hash(a || b)\nchunk_a = Bytes32(b'\\x01' + b'\\x00' * 31)\nchunk_b = Bytes32(b'\\x02' + b'\\x00' * 31)\nresult = merkleize([chunk_a, chunk_b])\nexpected = hash_nodes(chunk_a, chunk_b)\nprint(result.hex() == expected.hex())\nprint(result.hex())" "0000000000000000000000000000000000000000000000000000000000000000\n0100000000000000000000000000000000000000000000000000000000000000\n1\n1\n4\n8\nTrue\nff55c97976a840b4ced964ed49e3794594ba3f675238b5fd25d282b60f70a194\n"

-- Test L: hash_tree_root for Container with model_fields + mix_in_length
#eval assertPy "import hashlib\nfrom functools import singledispatch\nfrom abc import ABC, abstractmethod\nfrom typing import ClassVar, override\nfrom pydantic import BaseModel, ConfigDict\n\nBYTES_PER_CHUNK = 32\n\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n    @classmethod\n    @abstractmethod\n    def get_byte_length(cls):\n        pass\n\nclass StrictBaseModel(BaseModel):\n    model_config = ConfigDict(extra='forbid', frozen=True, strict=True)\n\nclass SSZModel(StrictBaseModel, SSZType):\n    pass\n\nclass BaseBytes(bytes, SSZType):\n    __slots__ = ()\n    LENGTH: ClassVar = 0\n    def __new__(cls, value=b''):\n        if isinstance(value, (bytes, bytearray)):\n            b = bytes(value)\n        elif isinstance(value, str):\n            b = bytes.fromhex(value)\n        else:\n            b = bytes(value)\n        return super().__new__(cls, b)\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return cls.LENGTH\n    def encode_bytes(self):\n        return bytes(self)\n    def hex(self):\n        return bytes(self).hex()\n    @classmethod\n    def zero(cls):\n        return cls(b'\\x00' * cls.LENGTH)\n\nclass Bytes32(BaseBytes):\n    LENGTH = 32\n\nZERO_HASH = Bytes32.zero()\n\nclass BaseUint(int, SSZType):\n    __slots__ = ()\n    BITS: ClassVar = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return cls.BITS // 8\n    def encode_bytes(self):\n        return int(self).to_bytes(self.get_byte_length(), 'little')\n\nclass Uint64(BaseUint):\n    BITS = 64\n\nclass Container(SSZModel):\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return 0\n\ndef hash_nodes(a, b):\n    return Bytes32(hashlib.sha256(bytes(a) + bytes(b)).digest())\n\ndef get_power_of_two_ceil(x):\n    if x <= 1:\n        return 1\n    return 1 << (x - 1).bit_length()\n\ndef pack_bytes(data):\n    if len(data) == 0:\n        return []\n    pad_len = BYTES_PER_CHUNK - (len(data) % BYTES_PER_CHUNK)\n    if pad_len < BYTES_PER_CHUNK:\n        data = data + b'\\x00' * pad_len\n    chunks = []\n    i = 0\n    while i < len(data):\n        chunks.append(Bytes32(data[i:i+BYTES_PER_CHUNK]))\n        i = i + BYTES_PER_CHUNK\n    return chunks\n\ndef merkleize(chunks, limit=None):\n    n = len(chunks)\n    if n == 0:\n        if limit is not None:\n            w = get_power_of_two_ceil(limit)\n            depth = (w - 1).bit_length() if w > 1 else 0\n            h = ZERO_HASH\n            for _ in range(depth):\n                h = hash_nodes(h, h)\n            return h\n        return ZERO_HASH\n    if limit is None:\n        width = get_power_of_two_ceil(n)\n    else:\n        width = get_power_of_two_ceil(limit)\n    if width == 1:\n        return chunks[0]\n    level = list(chunks)\n    subtree_size = 1\n    while subtree_size < width:\n        next_level = []\n        i = 0\n        while i < len(level):\n            left = level[i]\n            i = i + 1\n            if i < len(level):\n                right = level[i]\n                i = i + 1\n            else:\n                depth = (subtree_size - 1).bit_length() if subtree_size > 1 else 0\n                h = ZERO_HASH\n                for _ in range(depth):\n                    h = hash_nodes(h, h)\n                right = h\n            next_level.append(hash_nodes(left, right))\n        level = next_level\n        subtree_size = subtree_size * 2\n    return level[0]\n\ndef mix_in_length(root, length):\n    return hash_nodes(root, Bytes32(length.to_bytes(32, 'little')))\n\n@singledispatch\ndef hash_tree_root(value):\n    raise TypeError(f'unsupported type')\n\n@hash_tree_root.register\ndef _htr_uint(value: BaseUint):\n    return merkleize(pack_bytes(value.encode_bytes()))\n\n@hash_tree_root.register\ndef _htr_container(value: Container):\n    field_roots = []\n    for fname in type(value).model_fields:\n        field_val = getattr(value, fname)\n        field_roots.append(hash_tree_root(field_val))\n    return merkleize(field_roots)\n\n# Define a container\nclass Point(Container):\n    x: Uint64\n    y: Uint64\n    z: Uint64\n\np = Point(x=Uint64(10), y=Uint64(20), z=Uint64(30))\n\n# hash_tree_root of each field\nhtr_x = hash_tree_root(Uint64(10))\nhtr_y = hash_tree_root(Uint64(20))\nhtr_z = hash_tree_root(Uint64(30))\nprint(htr_x.hex()[:16])\nprint(htr_y.hex()[:16])\n\n# hash_tree_root of container = merkleize(field roots)\nhtr_p = hash_tree_root(p)\nprint(htr_p.hex())\n\n# Test mix_in_length\nroot = hash_tree_root(Uint64(1))\nmixed = mix_in_length(root, 3)\nprint(mixed.hex())" "0a00000000000000\n1400000000000000\nabcb0b59fa8ca75e762f69477bf6ad51a042ad77c8eb33dd9bb3c60fed5af194\ncaea92341df83aa8d4225099f16e86cbf457ec7ea97ccddb4ba5560062eee695\n"

-- ============================================================
-- Phase 9b: issubclass with tuple + bytes.join
-- ============================================================

-- Test M: issubclass(cls, tuple_of_types) and bytes.join
#eval assertPy "class A:\n    x = 1\nclass B(A):\n    y = 2\nclass C:\n    z = 3\nprint(issubclass(B, (A, C)))\nprint(issubclass(B, (C,)))\nprint(issubclass(bool, (int, str)))\nprint(issubclass(bool, (str, float)))\n# bytes.join\nresult = b''.join([b'\\x01', b'\\x02\\x03'])\nprint(result.hex())\nresult2 = b'-'.join([b'hello', b'world'])\nprint(result2.hex())" "True\nFalse\nTrue\nFalse\n010203\n68656c6c6f2d776f726c64\n"

-- ============================================================
-- Phase 9b: hash_tree_root for Boolean and BaseBytes
-- ============================================================

-- Test N: hash_tree_root for Boolean + BaseBytes (ByteVector)
#eval assertPy "import hashlib\nfrom functools import singledispatch\nfrom abc import ABC, abstractmethod\nfrom typing import ClassVar\n\nBYTES_PER_CHUNK = 32\n\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n\nclass BaseBytes(bytes, SSZType):\n    __slots__ = ()\n    LENGTH: ClassVar = 0\n    def __new__(cls, value=b''):\n        if isinstance(value, (bytes, bytearray)):\n            b = bytes(value)\n        elif isinstance(value, str):\n            b = bytes.fromhex(value)\n        else:\n            b = bytes(value)\n        return super().__new__(cls, b)\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    def encode_bytes(self):\n        return bytes(self)\n    def hex(self):\n        return bytes(self).hex()\n    @classmethod\n    def zero(cls):\n        return cls(b'\\x00' * cls.LENGTH)\n\nclass Bytes32(BaseBytes):\n    LENGTH = 32\n\nclass Bytes48(BaseBytes):\n    LENGTH = 48\n\nZERO_HASH = Bytes32.zero()\n\nclass BaseUint(int, SSZType):\n    __slots__ = ()\n    BITS: ClassVar = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return cls.BITS // 8\n    def encode_bytes(self):\n        return int(self).to_bytes(self.get_byte_length(), 'little')\n\nclass Boolean(int, SSZType):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    def encode_bytes(self):\n        return int(self).to_bytes(1, 'little')\n\ndef hash_nodes(a, b):\n    return Bytes32(hashlib.sha256(bytes(a) + bytes(b)).digest())\n\ndef get_power_of_two_ceil(x):\n    if x <= 1:\n        return 1\n    return 1 << (x - 1).bit_length()\n\ndef pack_bytes(data):\n    if len(data) == 0:\n        return []\n    pad_len = BYTES_PER_CHUNK - (len(data) % BYTES_PER_CHUNK)\n    if pad_len < BYTES_PER_CHUNK:\n        data = data + b'\\x00' * pad_len\n    chunks = []\n    i = 0\n    while i < len(data):\n        chunks.append(Bytes32(data[i:i+BYTES_PER_CHUNK]))\n        i = i + BYTES_PER_CHUNK\n    return chunks\n\ndef merkleize(chunks, limit=None):\n    n = len(chunks)\n    if n == 0:\n        if limit is not None:\n            w = get_power_of_two_ceil(limit)\n            depth = (w - 1).bit_length() if w > 1 else 0\n            h = ZERO_HASH\n            for _ in range(depth):\n                h = hash_nodes(h, h)\n            return h\n        return ZERO_HASH\n    if limit is None:\n        width = get_power_of_two_ceil(n)\n    else:\n        width = get_power_of_two_ceil(limit)\n    if width == 1:\n        return chunks[0]\n    level = list(chunks)\n    subtree_size = 1\n    while subtree_size < width:\n        next_level = []\n        i = 0\n        while i < len(level):\n            left = level[i]\n            i = i + 1\n            if i < len(level):\n                right = level[i]\n                i = i + 1\n            else:\n                depth = (subtree_size - 1).bit_length() if subtree_size > 1 else 0\n                h = ZERO_HASH\n                for _ in range(depth):\n                    h = hash_nodes(h, h)\n                right = h\n            next_level.append(hash_nodes(left, right))\n        level = next_level\n        subtree_size = subtree_size * 2\n    return level[0]\n\n@singledispatch\ndef hash_tree_root(value):\n    raise TypeError(f'unsupported type: {type(value).__name__}')\n\n@hash_tree_root.register\ndef _htr_uint(value: BaseUint):\n    return merkleize(pack_bytes(value.encode_bytes()))\n\n@hash_tree_root.register\ndef _htr_boolean(value: Boolean):\n    return merkleize(pack_bytes(value.encode_bytes()))\n\n@hash_tree_root.register\ndef _htr_basebytes(value: BaseBytes):\n    return merkleize(pack_bytes(value.encode_bytes()))\n\n# Boolean tests\nprint(hash_tree_root(Boolean(False)).hex())\nprint(hash_tree_root(Boolean(True)).hex())\n\n# BaseBytes: Bytes32 single chunk\nbv32 = Bytes32(b'\\xab' + b'\\x00' * 31)\nprint(hash_tree_root(bv32).hex())\n\n# BaseBytes: Bytes48 two chunks\nbv48 = Bytes48(bytes(range(48)))\nprint(hash_tree_root(bv48).hex())" "0000000000000000000000000000000000000000000000000000000000000000\n0100000000000000000000000000000000000000000000000000000000000000\nab00000000000000000000000000000000000000000000000000000000000000\nb976c9abe97b4f03d7e4058246713687379d2718a829ab66e2a93aa924e43c1d\n"

-- ============================================================
-- Phase 9b: hash_tree_root for BaseByteList (variable-size byte list)
-- ============================================================

-- Test O: hash_tree_root for BaseByteList with limit + mix_in_length
#eval assertPy "import hashlib\nfrom functools import singledispatch\nfrom abc import ABC, abstractmethod\nfrom typing import ClassVar, override\nfrom pydantic import BaseModel, ConfigDict\nfrom math import ceil\n\nBYTES_PER_CHUNK = 32\n\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n\nclass StrictBaseModel(BaseModel):\n    model_config = ConfigDict(extra='forbid', frozen=True, strict=True)\n\nclass SSZModel(StrictBaseModel, SSZType):\n    pass\n\nclass BaseBytes(bytes, SSZType):\n    __slots__ = ()\n    LENGTH: ClassVar = 0\n    def __new__(cls, value=b''):\n        if isinstance(value, (bytes, bytearray)):\n            b = bytes(value)\n        elif isinstance(value, str):\n            b = bytes.fromhex(value)\n        else:\n            b = bytes(value)\n        return super().__new__(cls, b)\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    def encode_bytes(self):\n        return bytes(self)\n    def hex(self):\n        return bytes(self).hex()\n    @classmethod\n    def zero(cls):\n        return cls(b'\\x00' * cls.LENGTH)\n\nclass Bytes32(BaseBytes):\n    LENGTH = 32\n\nZERO_HASH = Bytes32.zero()\n\nclass BaseByteList(SSZModel):\n    LIMIT: ClassVar = 0\n    data: bytes = b''\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return False\n    def encode_bytes(self):\n        return self.data\n\ndef hash_nodes(a, b):\n    return Bytes32(hashlib.sha256(bytes(a) + bytes(b)).digest())\n\ndef get_power_of_two_ceil(x):\n    if x <= 1:\n        return 1\n    return 1 << (x - 1).bit_length()\n\ndef pack_bytes(data):\n    if len(data) == 0:\n        return []\n    pad_len = BYTES_PER_CHUNK - (len(data) % BYTES_PER_CHUNK)\n    if pad_len < BYTES_PER_CHUNK:\n        data = data + b'\\x00' * pad_len\n    chunks = []\n    i = 0\n    while i < len(data):\n        chunks.append(Bytes32(data[i:i+BYTES_PER_CHUNK]))\n        i = i + BYTES_PER_CHUNK\n    return chunks\n\ndef merkleize(chunks, limit=None):\n    n = len(chunks)\n    if n == 0:\n        if limit is not None:\n            w = get_power_of_two_ceil(limit)\n            depth = (w - 1).bit_length() if w > 1 else 0\n            h = ZERO_HASH\n            for _ in range(depth):\n                h = hash_nodes(h, h)\n            return h\n        return ZERO_HASH\n    if limit is None:\n        width = get_power_of_two_ceil(n)\n    else:\n        width = get_power_of_two_ceil(limit)\n    if width == 1:\n        return chunks[0]\n    level = list(chunks)\n    subtree_size = 1\n    while subtree_size < width:\n        next_level = []\n        i = 0\n        while i < len(level):\n            left = level[i]\n            i = i + 1\n            if i < len(level):\n                right = level[i]\n                i = i + 1\n            else:\n                depth = (subtree_size - 1).bit_length() if subtree_size > 1 else 0\n                h = ZERO_HASH\n                for _ in range(depth):\n                    h = hash_nodes(h, h)\n                right = h\n            next_level.append(hash_nodes(left, right))\n        level = next_level\n        subtree_size = subtree_size * 2\n    return level[0]\n\ndef mix_in_length(root, length):\n    return hash_nodes(root, Bytes32(length.to_bytes(32, 'little')))\n\n@singledispatch\ndef hash_tree_root(value):\n    raise TypeError(f'unsupported type')\n\n@hash_tree_root.register\ndef _htr_bytelist(value: BaseByteList):\n    data = value.encode_bytes()\n    limit_chunks = ceil(type(value).LIMIT / BYTES_PER_CHUNK)\n    return mix_in_length(merkleize(pack_bytes(data), limit=limit_chunks), len(data))\n\nclass ByteList10(BaseByteList):\n    LIMIT = 10\nclass ByteList7(BaseByteList):\n    LIMIT = 7\nclass ByteList50(BaseByteList):\n    LIMIT = 50\nclass ByteList2048(BaseByteList):\n    LIMIT = 2048\n\n# Empty ByteList10\nprint(hash_tree_root(ByteList10(data=b'')).hex())\n# ByteList7 with 7 bytes\nprint(hash_tree_root(ByteList7(data=bytes([0,1,2,3,4,5,6]))).hex())\n# ByteList50 with 50 bytes\nprint(hash_tree_root(ByteList50(data=bytes(range(50)))).hex())\n# Empty ByteList2048 (64 chunks)\nprint(hash_tree_root(ByteList2048(data=b'')).hex())" "f5a5fd42d16a20302798ef6ed309979b43003d2320d9f0e8ea9831a92759fb4b\nec630fedf6a1af35b005bdf2148cc1093d62ea86ab6986fd61e7679e7df0af7e\nc1908a20653d62e8fc79c649c5c282fbce5178905e5e066578ff03317c3ffb40\nc9eece3e14d3c3db45c38bbf69a4cb7464981e2506d8424a0ba450dad9b9af30\n"

-- ============================================================
-- Phase 9b: hash_tree_root for BaseBitvector and BaseBitlist
-- ============================================================

-- Test P: pack_bits + hash_tree_root for BaseBitvector
#eval assertPy "import hashlib\nfrom functools import singledispatch\nfrom abc import ABC, abstractmethod\nfrom typing import ClassVar, override\nfrom pydantic import BaseModel, ConfigDict\n\nBYTES_PER_CHUNK = 32\nBITS_PER_BYTE = 8\nBITS_PER_CHUNK = 256\n\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n\nclass StrictBaseModel(BaseModel):\n    model_config = ConfigDict(extra='forbid', frozen=True, strict=True)\n\nclass SSZModel(StrictBaseModel, SSZType):\n    pass\n\nclass BaseBytes(bytes, SSZType):\n    __slots__ = ()\n    LENGTH: ClassVar = 0\n    def __new__(cls, value=b''):\n        if isinstance(value, (bytes, bytearray)):\n            b = bytes(value)\n        elif isinstance(value, str):\n            b = bytes.fromhex(value)\n        else:\n            b = bytes(value)\n        return super().__new__(cls, b)\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    def encode_bytes(self):\n        return bytes(self)\n    def hex(self):\n        return bytes(self).hex()\n    @classmethod\n    def zero(cls):\n        return cls(b'\\x00' * cls.LENGTH)\n\nclass Bytes32(BaseBytes):\n    LENGTH = 32\n\nZERO_HASH = Bytes32.zero()\n\nclass Boolean(int, SSZType):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n\nclass BaseBitvector(SSZModel):\n    LENGTH: ClassVar = 0\n    data: tuple = ()\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return True\n\ndef hash_nodes(a, b):\n    return Bytes32(hashlib.sha256(bytes(a) + bytes(b)).digest())\n\ndef get_power_of_two_ceil(x):\n    if x <= 1:\n        return 1\n    return 1 << (x - 1).bit_length()\n\ndef pack_bytes(data):\n    if len(data) == 0:\n        return []\n    pad_len = BYTES_PER_CHUNK - (len(data) % BYTES_PER_CHUNK)\n    if pad_len < BYTES_PER_CHUNK:\n        data = data + b'\\x00' * pad_len\n    chunks = []\n    i = 0\n    while i < len(data):\n        chunks.append(Bytes32(data[i:i+BYTES_PER_CHUNK]))\n        i = i + BYTES_PER_CHUNK\n    return chunks\n\ndef pack_bits(bools):\n    if not bools:\n        return []\n    byte_len = (len(bools) + (BITS_PER_BYTE - 1)) // BITS_PER_BYTE\n    arr = [0] * byte_len\n    for i, bit in enumerate(bools):\n        if bit:\n            arr[i // BITS_PER_BYTE] = arr[i // BITS_PER_BYTE] | (1 << (i % BITS_PER_BYTE))\n    raw = bytes(arr)\n    return pack_bytes(raw)\n\ndef merkleize(chunks, limit=None):\n    n = len(chunks)\n    if n == 0:\n        if limit is not None:\n            w = get_power_of_two_ceil(limit)\n            depth = (w - 1).bit_length() if w > 1 else 0\n            h = ZERO_HASH\n            for _ in range(depth):\n                h = hash_nodes(h, h)\n            return h\n        return ZERO_HASH\n    if limit is None:\n        width = get_power_of_two_ceil(n)\n    else:\n        width = get_power_of_two_ceil(limit)\n    if width == 1:\n        return chunks[0]\n    level = list(chunks)\n    subtree_size = 1\n    while subtree_size < width:\n        next_level = []\n        i = 0\n        while i < len(level):\n            left = level[i]\n            i = i + 1\n            if i < len(level):\n                right = level[i]\n                i = i + 1\n            else:\n                depth = (subtree_size - 1).bit_length() if subtree_size > 1 else 0\n                h = ZERO_HASH\n                for _ in range(depth):\n                    h = hash_nodes(h, h)\n                right = h\n            next_level.append(hash_nodes(left, right))\n        level = next_level\n        subtree_size = subtree_size * 2\n    return level[0]\n\ndef mix_in_length(root, length):\n    return hash_nodes(root, Bytes32(length.to_bytes(32, 'little')))\n\n@singledispatch\ndef hash_tree_root(value):\n    raise TypeError(f'unsupported type')\n\n@hash_tree_root.register\ndef _htr_bitvector(value: BaseBitvector):\n    limit = (type(value).LENGTH + BITS_PER_CHUNK - 1) // BITS_PER_CHUNK\n    return merkleize(pack_bits(tuple(bool(b) for b in value.data)), limit=limit)\n\n# Bitvector[8]: bits (1,1,0,1,0,1,0,0) -> 0x2b\nclass BV8(BaseBitvector):\n    LENGTH = 8\nbv8 = BV8(data=(Boolean(1), Boolean(1), Boolean(0), Boolean(1), Boolean(0), Boolean(1), Boolean(0), Boolean(0)))\nprint(hash_tree_root(bv8).hex())\n\n# Bitvector[4]: bits (0,1,0,1) -> 0x0a\nclass BV4(BaseBitvector):\n    LENGTH = 4\nbv4 = BV4(data=(Boolean(0), Boolean(1), Boolean(0), Boolean(1)))\nprint(hash_tree_root(bv4).hex())\n\n# Bitvector[3]: bits (0,1,0) -> 0x02\nclass BV3(BaseBitvector):\n    LENGTH = 3\nbv3 = BV3(data=(Boolean(0), Boolean(1), Boolean(0)))\nprint(hash_tree_root(bv3).hex())" "2b00000000000000000000000000000000000000000000000000000000000000\n0a00000000000000000000000000000000000000000000000000000000000000\n0200000000000000000000000000000000000000000000000000000000000000\n"

-- Test Q: hash_tree_root for BaseBitlist (pack_bits + mix_in_length)
#eval assertPy "import hashlib\nfrom functools import singledispatch\nfrom abc import ABC, abstractmethod\nfrom typing import ClassVar, override\nfrom pydantic import BaseModel, ConfigDict\n\nBYTES_PER_CHUNK = 32\nBITS_PER_BYTE = 8\nBITS_PER_CHUNK = 256\n\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n\nclass StrictBaseModel(BaseModel):\n    model_config = ConfigDict(extra='forbid', frozen=True, strict=True)\n\nclass SSZModel(StrictBaseModel, SSZType):\n    pass\n\nclass BaseBytes(bytes, SSZType):\n    __slots__ = ()\n    LENGTH: ClassVar = 0\n    def __new__(cls, value=b''):\n        if isinstance(value, (bytes, bytearray)):\n            b = bytes(value)\n        elif isinstance(value, str):\n            b = bytes.fromhex(value)\n        else:\n            b = bytes(value)\n        return super().__new__(cls, b)\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    def encode_bytes(self):\n        return bytes(self)\n    def hex(self):\n        return bytes(self).hex()\n    @classmethod\n    def zero(cls):\n        return cls(b'\\x00' * cls.LENGTH)\n\nclass Bytes32(BaseBytes):\n    LENGTH = 32\n\nZERO_HASH = Bytes32.zero()\n\nclass Boolean(int, SSZType):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n\nclass BaseBitlist(SSZModel):\n    LIMIT: ClassVar = 0\n    data: tuple = ()\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return False\n\ndef hash_nodes(a, b):\n    return Bytes32(hashlib.sha256(bytes(a) + bytes(b)).digest())\n\ndef get_power_of_two_ceil(x):\n    if x <= 1:\n        return 1\n    return 1 << (x - 1).bit_length()\n\ndef pack_bytes(data):\n    if len(data) == 0:\n        return []\n    pad_len = BYTES_PER_CHUNK - (len(data) % BYTES_PER_CHUNK)\n    if pad_len < BYTES_PER_CHUNK:\n        data = data + b'\\x00' * pad_len\n    chunks = []\n    i = 0\n    while i < len(data):\n        chunks.append(Bytes32(data[i:i+BYTES_PER_CHUNK]))\n        i = i + BYTES_PER_CHUNK\n    return chunks\n\ndef pack_bits(bools):\n    if not bools:\n        return []\n    byte_len = (len(bools) + (BITS_PER_BYTE - 1)) // BITS_PER_BYTE\n    arr = [0] * byte_len\n    for i, bit in enumerate(bools):\n        if bit:\n            arr[i // BITS_PER_BYTE] = arr[i // BITS_PER_BYTE] | (1 << (i % BITS_PER_BYTE))\n    raw = bytes(arr)\n    return pack_bytes(raw)\n\ndef merkleize(chunks, limit=None):\n    n = len(chunks)\n    if n == 0:\n        if limit is not None:\n            w = get_power_of_two_ceil(limit)\n            depth = (w - 1).bit_length() if w > 1 else 0\n            h = ZERO_HASH\n            for _ in range(depth):\n                h = hash_nodes(h, h)\n            return h\n        return ZERO_HASH\n    if limit is None:\n        width = get_power_of_two_ceil(n)\n    else:\n        width = get_power_of_two_ceil(limit)\n    if width == 1:\n        return chunks[0]\n    level = list(chunks)\n    subtree_size = 1\n    while subtree_size < width:\n        next_level = []\n        i = 0\n        while i < len(level):\n            left = level[i]\n            i = i + 1\n            if i < len(level):\n                right = level[i]\n                i = i + 1\n            else:\n                depth = (subtree_size - 1).bit_length() if subtree_size > 1 else 0\n                h = ZERO_HASH\n                for _ in range(depth):\n                    h = hash_nodes(h, h)\n                right = h\n            next_level.append(hash_nodes(left, right))\n        level = next_level\n        subtree_size = subtree_size * 2\n    return level[0]\n\ndef mix_in_length(root, length):\n    return hash_nodes(root, Bytes32(length.to_bytes(32, 'little')))\n\n@singledispatch\ndef hash_tree_root(value):\n    raise TypeError(f'unsupported type')\n\n@hash_tree_root.register\ndef _htr_bitlist(value: BaseBitlist):\n    limit = (type(value).LIMIT + BITS_PER_CHUNK - 1) // BITS_PER_CHUNK\n    return mix_in_length(merkleize(pack_bits(tuple(bool(b) for b in value.data)), limit=limit), len(value.data))\n\n# Bitlist[8]\nclass BL8(BaseBitlist):\n    LIMIT = 8\nbl8 = BL8(data=(Boolean(1), Boolean(1), Boolean(0), Boolean(1), Boolean(0), Boolean(1), Boolean(0), Boolean(0)))\nprint(hash_tree_root(bl8).hex())\n\n# Bitlist[4]\nclass BL4(BaseBitlist):\n    LIMIT = 4\nbl4 = BL4(data=(Boolean(0), Boolean(1), Boolean(0), Boolean(1)))\nprint(hash_tree_root(bl4).hex())\n\n# Bitlist[3]\nclass BL3(BaseBitlist):\n    LIMIT = 3\nbl3 = BL3(data=(Boolean(0), Boolean(1), Boolean(0)))\nprint(hash_tree_root(bl3).hex())" "e3c680050925d8be5b3c4f4c2b5619010db0015f1bfed7643c0b4fc3700d2d15\n62a896c7f7f5be6f6d17063247bf1d2cd0410dbd1fdc1400c097d4e09574cca2\n0094579cfc7b716038d416a311465309bea202baa922b224a7b08f01599642fb\n"

-- ============================================================
-- Phase 9b: hash_tree_root for SSZVector and SSZList
-- ============================================================

-- Test R: hash_tree_root for SSZVector (basic elements via b"".join + issubclass tuple)
#eval assertPy "import hashlib\nfrom functools import singledispatch\nfrom abc import ABC, abstractmethod\nfrom typing import ClassVar, override\nfrom pydantic import BaseModel, ConfigDict\n\nBYTES_PER_CHUNK = 32\n\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n\nclass StrictBaseModel(BaseModel):\n    model_config = ConfigDict(extra='forbid', frozen=True, strict=True)\n\nclass SSZModel(StrictBaseModel, SSZType):\n    pass\n\nclass BaseBytes(bytes, SSZType):\n    __slots__ = ()\n    LENGTH: ClassVar = 0\n    def __new__(cls, value=b''):\n        if isinstance(value, (bytes, bytearray)):\n            b = bytes(value)\n        elif isinstance(value, str):\n            b = bytes.fromhex(value)\n        else:\n            b = bytes(value)\n        return super().__new__(cls, b)\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return cls.LENGTH\n    def encode_bytes(self):\n        return bytes(self)\n    def hex(self):\n        return bytes(self).hex()\n    @classmethod\n    def zero(cls):\n        return cls(b'\\x00' * cls.LENGTH)\n\nclass Bytes32(BaseBytes):\n    LENGTH = 32\n\nZERO_HASH = Bytes32.zero()\n\nclass BaseUint(int, SSZType):\n    __slots__ = ()\n    BITS: ClassVar = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return cls.BITS // 8\n    def encode_bytes(self):\n        return int(self).to_bytes(self.get_byte_length(), 'little')\n\nclass Uint16(BaseUint):\n    BITS = 16\n\nclass Boolean(int, SSZType):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return 1\n\nclass SSZVector(SSZModel):\n    ELEMENT_TYPE: ClassVar = None\n    LENGTH: ClassVar = 0\n    data: tuple = ()\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return True\n    def __iter__(self):\n        return iter(self.data)\n    def __len__(self):\n        return len(self.data)\n\ndef hash_nodes(a, b):\n    return Bytes32(hashlib.sha256(bytes(a) + bytes(b)).digest())\n\ndef get_power_of_two_ceil(x):\n    if x <= 1:\n        return 1\n    return 1 << (x - 1).bit_length()\n\ndef pack_bytes(data):\n    if len(data) == 0:\n        return []\n    pad_len = BYTES_PER_CHUNK - (len(data) % BYTES_PER_CHUNK)\n    if pad_len < BYTES_PER_CHUNK:\n        data = data + b'\\x00' * pad_len\n    chunks = []\n    i = 0\n    while i < len(data):\n        chunks.append(Bytes32(data[i:i+BYTES_PER_CHUNK]))\n        i = i + BYTES_PER_CHUNK\n    return chunks\n\ndef merkleize(chunks, limit=None):\n    n = len(chunks)\n    if n == 0:\n        if limit is not None:\n            w = get_power_of_two_ceil(limit)\n            depth = (w - 1).bit_length() if w > 1 else 0\n            h = ZERO_HASH\n            for _ in range(depth):\n                h = hash_nodes(h, h)\n            return h\n        return ZERO_HASH\n    if limit is None:\n        width = get_power_of_two_ceil(n)\n    else:\n        width = get_power_of_two_ceil(limit)\n    if width == 1:\n        return chunks[0]\n    level = list(chunks)\n    subtree_size = 1\n    while subtree_size < width:\n        next_level = []\n        i = 0\n        while i < len(level):\n            left = level[i]\n            i = i + 1\n            if i < len(level):\n                right = level[i]\n                i = i + 1\n            else:\n                depth = (subtree_size - 1).bit_length() if subtree_size > 1 else 0\n                h = ZERO_HASH\n                for _ in range(depth):\n                    h = hash_nodes(h, h)\n                right = h\n            next_level.append(hash_nodes(left, right))\n        level = next_level\n        subtree_size = subtree_size * 2\n    return level[0]\n\n@singledispatch\ndef hash_tree_root(value):\n    raise TypeError(f'unsupported type')\n\n@hash_tree_root.register\ndef _htr_uint(value: BaseUint):\n    return merkleize(pack_bytes(value.encode_bytes()))\n\n@hash_tree_root.register\ndef _htr_vector(value: SSZVector):\n    elem_t = type(value).ELEMENT_TYPE\n    length = type(value).LENGTH\n    if issubclass(elem_t, (BaseUint, Boolean)):\n        elem_size = elem_t.get_byte_length()\n        limit_chunks = (length * elem_size + BYTES_PER_CHUNK - 1) // BYTES_PER_CHUNK\n        return merkleize(pack_bytes(b''.join(e.encode_bytes() for e in value)), limit=limit_chunks)\n    return merkleize([hash_tree_root(e) for e in value], limit=length)\n\n# Vector of Uint16, length=2\nclass Vec2U16(SSZVector):\n    ELEMENT_TYPE = Uint16\n    LENGTH = 2\nv = Vec2U16(data=(Uint16(0x4567), Uint16(0x0123)))\nprint(hash_tree_root(v).hex())\n\n# Vector of Uint16, length=3\nclass Vec3U16(SSZVector):\n    ELEMENT_TYPE = Uint16\n    LENGTH = 3\nv3 = Vec3U16(data=(Uint16(0xAABB), Uint16(0xC0AD), Uint16(0xEEFF)))\nprint(hash_tree_root(v3).hex())" "6745230100000000000000000000000000000000000000000000000000000000\nbbaaadc0ffee0000000000000000000000000000000000000000000000000000\n"

-- Test S: hash_tree_root for SSZList (basic elements + mix_in_length)
#eval assertPy "import hashlib\nfrom functools import singledispatch\nfrom abc import ABC, abstractmethod\nfrom typing import ClassVar, override\nfrom pydantic import BaseModel, ConfigDict\n\nBYTES_PER_CHUNK = 32\n\nclass SSZType(ABC):\n    @classmethod\n    @abstractmethod\n    def is_fixed_size(cls):\n        pass\n\nclass StrictBaseModel(BaseModel):\n    model_config = ConfigDict(extra='forbid', frozen=True, strict=True)\n\nclass SSZModel(StrictBaseModel, SSZType):\n    pass\n\nclass BaseBytes(bytes, SSZType):\n    __slots__ = ()\n    LENGTH: ClassVar = 0\n    def __new__(cls, value=b''):\n        if isinstance(value, (bytes, bytearray)):\n            b = bytes(value)\n        elif isinstance(value, str):\n            b = bytes.fromhex(value)\n        else:\n            b = bytes(value)\n        return super().__new__(cls, b)\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return cls.LENGTH\n    def encode_bytes(self):\n        return bytes(self)\n    def hex(self):\n        return bytes(self).hex()\n    @classmethod\n    def zero(cls):\n        return cls(b'\\x00' * cls.LENGTH)\n\nclass Bytes32(BaseBytes):\n    LENGTH = 32\n\nZERO_HASH = Bytes32.zero()\n\nclass BaseUint(int, SSZType):\n    __slots__ = ()\n    BITS: ClassVar = 64\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return cls.BITS // 8\n    def encode_bytes(self):\n        return int(self).to_bytes(self.get_byte_length(), 'little')\n\nclass Uint16(BaseUint):\n    BITS = 16\nclass Uint32(BaseUint):\n    BITS = 32\n\nclass Boolean(int, SSZType):\n    def __new__(cls, value):\n        return super().__new__(cls, int(value))\n    @classmethod\n    def is_fixed_size(cls):\n        return True\n    @classmethod\n    def get_byte_length(cls):\n        return 1\n\nclass SSZList(SSZModel):\n    ELEMENT_TYPE: ClassVar = None\n    LIMIT: ClassVar = 0\n    data: tuple = ()\n    @classmethod\n    @override\n    def is_fixed_size(cls):\n        return False\n    def __iter__(self):\n        return iter(self.data)\n    def __len__(self):\n        return len(self.data)\n\ndef hash_nodes(a, b):\n    return Bytes32(hashlib.sha256(bytes(a) + bytes(b)).digest())\n\ndef get_power_of_two_ceil(x):\n    if x <= 1:\n        return 1\n    return 1 << (x - 1).bit_length()\n\ndef pack_bytes(data):\n    if len(data) == 0:\n        return []\n    pad_len = BYTES_PER_CHUNK - (len(data) % BYTES_PER_CHUNK)\n    if pad_len < BYTES_PER_CHUNK:\n        data = data + b'\\x00' * pad_len\n    chunks = []\n    i = 0\n    while i < len(data):\n        chunks.append(Bytes32(data[i:i+BYTES_PER_CHUNK]))\n        i = i + BYTES_PER_CHUNK\n    return chunks\n\ndef merkleize(chunks, limit=None):\n    n = len(chunks)\n    if n == 0:\n        if limit is not None:\n            w = get_power_of_two_ceil(limit)\n            depth = (w - 1).bit_length() if w > 1 else 0\n            h = ZERO_HASH\n            for _ in range(depth):\n                h = hash_nodes(h, h)\n            return h\n        return ZERO_HASH\n    if limit is None:\n        width = get_power_of_two_ceil(n)\n    else:\n        width = get_power_of_two_ceil(limit)\n    if width == 1:\n        return chunks[0]\n    level = list(chunks)\n    subtree_size = 1\n    while subtree_size < width:\n        next_level = []\n        i = 0\n        while i < len(level):\n            left = level[i]\n            i = i + 1\n            if i < len(level):\n                right = level[i]\n                i = i + 1\n            else:\n                depth = (subtree_size - 1).bit_length() if subtree_size > 1 else 0\n                h = ZERO_HASH\n                for _ in range(depth):\n                    h = hash_nodes(h, h)\n                right = h\n            next_level.append(hash_nodes(left, right))\n        level = next_level\n        subtree_size = subtree_size * 2\n    return level[0]\n\ndef mix_in_length(root, length):\n    return hash_nodes(root, Bytes32(length.to_bytes(32, 'little')))\n\n@singledispatch\ndef hash_tree_root(value):\n    raise TypeError(f'unsupported type')\n\n@hash_tree_root.register\ndef _htr_uint(value: BaseUint):\n    return merkleize(pack_bytes(value.encode_bytes()))\n\n@hash_tree_root.register\ndef _htr_basebytes(value: BaseBytes):\n    return merkleize(pack_bytes(value.encode_bytes()))\n\n@hash_tree_root.register\ndef _htr_list(value: SSZList):\n    elem_t = type(value).ELEMENT_TYPE\n    limit = type(value).LIMIT\n    if issubclass(elem_t, (BaseUint, Boolean)):\n        elem_size = elem_t.get_byte_length()\n        limit_chunks = (limit * elem_size + BYTES_PER_CHUNK - 1) // BYTES_PER_CHUNK\n        root = merkleize(pack_bytes(b''.join(e.encode_bytes() for e in value)), limit=limit_chunks)\n    else:\n        root = merkleize([hash_tree_root(e) for e in value], limit=limit)\n    return mix_in_length(root, len(value))\n\n# List[Uint16, 32] with 3 elements\nclass U16List32(SSZList):\n    ELEMENT_TYPE = Uint16\n    LIMIT = 32\nls1 = U16List32(data=(Uint16(0xAABB), Uint16(0xC0AD), Uint16(0xEEFF)))\nprint(hash_tree_root(ls1).hex())\n\n# List[Uint32, 128] with 3 elements\nclass U32List128(SSZList):\n    ELEMENT_TYPE = Uint32\n    LIMIT = 128\nls2 = U32List128(data=(Uint32(0xAABB), Uint32(0xC0AD), Uint32(0xEEFF)))\nprint(hash_tree_root(ls2).hex())\n\n# List[Bytes32, 32] with 3 composite elements\nclass B32List32(SSZList):\n    ELEMENT_TYPE = Bytes32\n    LIMIT = 32\nls3 = B32List32(data=(Bytes32(b'\\xbb\\xaa' + b'\\x00' * 30), Bytes32(b'\\xad\\xc0' + b'\\x00' * 30), Bytes32(b'\\xff\\xee' + b'\\x00' * 30)))\nprint(hash_tree_root(ls3).hex())" "b2a7a399cd59bdb140436a0aab2b4523bf1af045c3c2fb165dc041d540c2acad\n2b1c23bc580de3a18f6b321af7092d71c5ec05087799ebea1656b483278a638c\n3d9e2bd8231a418ff3092162244bcdb1aab981c8182c1977a53dc9072de5f205\n"
