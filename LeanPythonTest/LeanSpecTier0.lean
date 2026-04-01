import LeanPythonTest.LeanSpecHelpers

set_option autoImplicit false

open LeanPythonTest.LeanSpecHelpers

-- ============================================================
-- Tier 0: types/constants.py + types/exceptions.py
-- ============================================================

-- Test A: Load real constants.py and verify OFFSET_BYTE_LENGTH
#eval assertLeanSpec
  [("lean_spec/types/constants.py", "types/constants.py")]
  [("lean_spec/__init__.py", ""),
   ("lean_spec/types/__init__.py", "from .constants import OFFSET_BYTE_LENGTH")]
  "from lean_spec.types.constants import OFFSET_BYTE_LENGTH
print(OFFSET_BYTE_LENGTH)
"
  "4\n"

-- Test B: Load real exceptions.py and verify exception hierarchy
#eval assertLeanSpec
  [("lean_spec/types/constants.py", "types/constants.py"),
   ("lean_spec/types/exceptions.py", "types/exceptions.py")]
  [("lean_spec/__init__.py", ""),
   ("lean_spec/types/__init__.py", "from .constants import OFFSET_BYTE_LENGTH\nfrom .exceptions import SSZError, SSZTypeError, SSZValueError, SSZSerializationError")]
  "from lean_spec.types.exceptions import SSZError, SSZTypeError, SSZValueError, SSZSerializationError
print(issubclass(SSZTypeError, SSZError))
print(issubclass(SSZValueError, SSZError))
print(issubclass(SSZSerializationError, SSZError))
try:
    raise SSZTypeError('test error')
except SSZError as e:
    print(type(e).__name__)
    print(str(e))
"
  "True\nTrue\nTrue\nSSZTypeError\ntest error\n"
