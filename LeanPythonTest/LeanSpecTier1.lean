import LeanPythonTest.LeanSpecHelpers

set_option autoImplicit false

open LeanPythonTest.LeanSpecHelpers

-- ============================================================
-- Tier 1-2: types/base.py + types/ssz_base.py
-- ============================================================

-- Test C: Load real base.py, create a CamelModel subclass
#eval assertLeanSpec
  [("lean_spec/types/base.py", "types/base.py")]
  [("lean_spec/__init__.py", ""),
   ("lean_spec/types/__init__.py", "from .base import CamelModel, StrictBaseModel")]
  "from lean_spec.types.base import CamelModel, StrictBaseModel
print(type(CamelModel).__name__)
print(type(StrictBaseModel).__name__)

class MyModel(StrictBaseModel):
    x: int = 10
    y: str = 'hello'

m = MyModel()
print(m.x)
print(m.y)
"
  "type\ntype\n10\nhello\n"

-- Test D: StrictBaseModel ConfigDict inheritance (frozen + extra=forbid)
#eval assertLeanSpec
  [("lean_spec/types/base.py", "types/base.py")]
  [("lean_spec/__init__.py", ""),
   ("lean_spec/types/__init__.py", "from .base import CamelModel, StrictBaseModel")]
  "from lean_spec.types.base import StrictBaseModel

class Point(StrictBaseModel):
    x: int = 0
    y: int = 0

p = Point(x=1, y=2)
print(p.x)
print(p.y)

# Test frozen (should raise on assignment)
try:
    p.x = 99
    print('ERROR: should be frozen')
except Exception:
    print('frozen ok')

# Test model_dump with by_alias
d = p.model_dump(mode='json', by_alias=True)
print(d)
"
  "1\n2\nfrozen ok\n{'x': 1, 'y': 2}\n"

-- Test E: Load real ssz_base.py, verify SSZType and SSZModel classes
#eval assertLeanSpec
  [("lean_spec/types/base.py", "types/base.py"),
   ("lean_spec/types/ssz_base.py", "types/ssz_base.py")]
  [("lean_spec/__init__.py", ""),
   ("lean_spec/types/__init__.py",
    "from .base import CamelModel, StrictBaseModel\nfrom .ssz_base import SSZType, SSZModel")]
  "from lean_spec.types.ssz_base import SSZType, SSZModel
print(type(SSZType).__name__)
print(type(SSZModel).__name__)
# SSZModel should have __len__ from the class definition
print(hasattr(SSZModel, '__len__'))
"
  "type\ntype\nTrue\n"
