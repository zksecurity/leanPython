import LeanPythonTest.LeanSpecHelpers

set_option autoImplicit false

open LeanPythonTest.LeanSpecHelpers

-- ============================================================
-- Tier 4: types/container.py (Container with serialize/deserialize)
-- ============================================================

-- Test H: Load real container.py, create a Container subclass, serialize/deserialize round-trip
#eval assertLeanSpec
  [("lean_spec/types/base.py", "types/base.py"),
   ("lean_spec/types/ssz_base.py", "types/ssz_base.py"),
   ("lean_spec/types/constants.py", "types/constants.py"),
   ("lean_spec/types/exceptions.py", "types/exceptions.py"),
   ("lean_spec/types/uint.py", "types/uint.py"),
   ("lean_spec/types/boolean.py", "types/boolean.py"),
   ("lean_spec/types/container.py", "types/container.py")]
  [("lean_spec/__init__.py", ""),
   ("lean_spec/types/__init__.py",
    "from .constants import OFFSET_BYTE_LENGTH\nfrom .exceptions import SSZError, SSZTypeError, SSZValueError, SSZSerializationError\nfrom .base import CamelModel, StrictBaseModel\nfrom .ssz_base import SSZType, SSZModel\nfrom .uint import BaseUint, Uint8, Uint16, Uint32, Uint64\nfrom .boolean import Boolean\nfrom .container import Container")]
  "from lean_spec.types.uint import Uint64, Uint32
from lean_spec.types.boolean import Boolean
from lean_spec.types.container import Container

class Checkpoint(Container):
    epoch: Uint64 = Uint64(0)
    active: Boolean = Boolean(False)

# Create instance
cp = Checkpoint(epoch=Uint64(42), active=Boolean(True))
print(int(cp.epoch))
print(int(cp.active))

# is_fixed_size
print(Checkpoint.is_fixed_size())

# get_byte_length
print(Checkpoint.get_byte_length())

# Serialize
data = cp.encode_bytes()
print(len(data))

# Deserialize
cp2 = Checkpoint.decode_bytes(data)
print(int(cp2.epoch))
print(int(cp2.active))
print(cp2 == cp)
"
  "42\n1\nTrue\n9\n9\n42\n1\nTrue\n"
