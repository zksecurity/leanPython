import LeanPythonTest.LeanSpecHelpers

set_option autoImplicit false

open LeanPythonTest.LeanSpecHelpers

-- ============================================================
-- Tier 3: types/uint.py + types/boolean.py
-- ============================================================

-- Test F: Load real uint.py, create Uint64 instances, test arithmetic
#eval assertLeanSpec
  [("lean_spec/types/base.py", "types/base.py"),
   ("lean_spec/types/ssz_base.py", "types/ssz_base.py"),
   ("lean_spec/types/constants.py", "types/constants.py"),
   ("lean_spec/types/exceptions.py", "types/exceptions.py"),
   ("lean_spec/types/uint.py", "types/uint.py")]
  [("lean_spec/__init__.py", ""),
   ("lean_spec/types/__init__.py",
    "from .constants import OFFSET_BYTE_LENGTH\nfrom .exceptions import SSZError, SSZTypeError, SSZValueError, SSZSerializationError\nfrom .base import CamelModel, StrictBaseModel\nfrom .ssz_base import SSZType, SSZModel\nfrom .uint import BaseUint, Uint8, Uint16, Uint32, Uint64")]
  "from lean_spec.types.uint import Uint64, Uint32, Uint8

# Basic construction
a = Uint64(42)
print(int(a))
print(type(a).__name__)

# Arithmetic between same types
b = Uint64(8)
print(int(a + b))
print(int(a - b))
print(int(a * b))

# Comparison
print(a > b)
print(a == Uint64(42))

# encode_bytes (little-endian)
data = a.encode_bytes()
print(len(data))
print(data[0])

# decode_bytes round-trip
c = Uint64.decode_bytes(data)
print(int(c))
print(c == a)

# Validation: reject out-of-range
try:
    Uint8(256)
    print('ERROR')
except Exception as e:
    print(type(e).__name__)

# Validation: reject bool
try:
    Uint64(True)
    print('ERROR')
except Exception as e:
    print(type(e).__name__)
"
  "42\nUint64\n50\n34\n336\nTrue\nTrue\n8\n42\n42\nTrue\nSSZValueError\nSSZTypeError\n"

-- Test G: Load real boolean.py, test Boolean type
#eval assertLeanSpec
  [("lean_spec/types/base.py", "types/base.py"),
   ("lean_spec/types/ssz_base.py", "types/ssz_base.py"),
   ("lean_spec/types/constants.py", "types/constants.py"),
   ("lean_spec/types/exceptions.py", "types/exceptions.py"),
   ("lean_spec/types/boolean.py", "types/boolean.py")]
  [("lean_spec/__init__.py", ""),
   ("lean_spec/types/__init__.py",
    "from .constants import OFFSET_BYTE_LENGTH\nfrom .exceptions import SSZError, SSZTypeError, SSZValueError, SSZSerializationError\nfrom .base import CamelModel, StrictBaseModel\nfrom .ssz_base import SSZType, SSZModel\nfrom .boolean import Boolean")]
  "from lean_spec.types.boolean import Boolean

b1 = Boolean(True)
b0 = Boolean(0)
print(int(b1))
print(int(b0))
print(type(b1).__name__)

# encode_bytes
data = b1.encode_bytes()
print(len(data))
print(data[0])

# decode_bytes round-trip
b2 = Boolean.decode_bytes(data)
print(int(b2))
print(b2 == b1)

# Validation: reject 2
try:
    Boolean(2)
    print('ERROR')
except Exception as e:
    print(type(e).__name__)
"
  "1\n0\nBoolean\n1\n1\n1\nTrue\nSSZValueError\n"
