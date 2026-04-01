#!/bin/bash
# End-to-end integration tests against real leanSpec Python files.
# Runs via the compiled leanPython executable (fast, ~seconds per test).
# Usage: ./test_leanspec.sh

set -e
EXE=".lake/build/bin/leanPython"
SRC="references/leanSpec/src/lean_spec"
PASS=0
FAIL=0

run_test() {
  local name="$1" dir="$2" main="$3" expected="$4"
  local actual
  actual=$(timeout 30 "$EXE" "$dir/main.py" 2>&1) || true
  if [ "$actual" = "$expected" ]; then
    echo "  PASS: $name"
    PASS=$((PASS+1))
  else
    echo "  FAIL: $name"
    echo "    Expected: $(echo "$expected" | head -3)"
    echo "    Got:      $(echo "$actual" | head -3)"
    FAIL=$((FAIL+1))
  fi
}

setup_types_dir() {
  local dir="$1"
  mkdir -p "$dir/lean_spec/types"
  touch "$dir/lean_spec/__init__.py"
  # Copy real leanSpec files
  for f in constants.py exceptions.py base.py ssz_base.py uint.py boolean.py container.py; do
    [ -f "$SRC/types/$f" ] && cp "$SRC/types/$f" "$dir/lean_spec/types/"
  done
}

setup_types_dir_tier5() {
  local dir="$1"
  setup_types_dir "$dir"
  for f in byte_arrays.py bitfields.py; do
    [ -f "$SRC/types/$f" ] && cp "$SRC/types/$f" "$dir/lean_spec/types/"
  done
}

setup_types_dir_tier6() {
  local dir="$1"
  setup_types_dir_tier5 "$dir"
  for f in collections.py union.py; do
    [ -f "$SRC/types/$f" ] && cp "$SRC/types/$f" "$dir/lean_spec/types/"
  done
}

# ============================================================
echo "=== Tier 0: constants.py + exceptions.py ==="

DIR=$(mktemp -d)
setup_types_dir "$DIR"
echo "from .constants import OFFSET_BYTE_LENGTH" > "$DIR/lean_spec/types/__init__.py"
cat > "$DIR/main.py" << 'EOF'
from lean_spec.types.constants import OFFSET_BYTE_LENGTH
print(OFFSET_BYTE_LENGTH)
EOF
run_test "constants.py OFFSET_BYTE_LENGTH" "$DIR" "main.py" "4"

DIR=$(mktemp -d)
setup_types_dir "$DIR"
echo "from .exceptions import SSZError, SSZTypeError" > "$DIR/lean_spec/types/__init__.py"
cat > "$DIR/main.py" << 'EOF'
from lean_spec.types.exceptions import SSZTypeError, SSZError
print(issubclass(SSZTypeError, SSZError))
try:
    raise SSZTypeError('test')
except SSZError as e:
    print(type(e).__name__)
EOF
run_test "exceptions.py hierarchy" "$DIR" "main.py" "True
SSZTypeError"

# ============================================================
echo "=== Tier 1: base.py (CamelModel, StrictBaseModel) ==="

DIR=$(mktemp -d)
setup_types_dir "$DIR"
echo "from .base import CamelModel, StrictBaseModel" > "$DIR/lean_spec/types/__init__.py"
cat > "$DIR/main.py" << 'EOF'
from lean_spec.types.base import StrictBaseModel
class Point(StrictBaseModel):
    x: int = 0
    y: int = 0
p = Point(x=1, y=2)
print(p.x)
print(p.y)
try:
    p.x = 99
except Exception:
    print('frozen ok')
EOF
run_test "StrictBaseModel frozen" "$DIR" "main.py" "1
2
frozen ok"

# ============================================================
echo "=== Tier 2: ssz_base.py (SSZType, SSZModel) ==="

DIR=$(mktemp -d)
setup_types_dir "$DIR"
echo "from .base import StrictBaseModel
from .ssz_base import SSZType, SSZModel" > "$DIR/lean_spec/types/__init__.py"
cat > "$DIR/main.py" << 'EOF'
from lean_spec.types.ssz_base import SSZType, SSZModel
print(type(SSZType).__name__)
print(type(SSZModel).__name__)
EOF
run_test "SSZType + SSZModel classes" "$DIR" "main.py" "type
type"

# ============================================================
echo "=== Tier 3: uint.py (BaseUint, Uint64) ==="

DIR=$(mktemp -d)
setup_types_dir "$DIR"
cat > "$DIR/lean_spec/types/__init__.py" << 'INITEOF'
from .constants import OFFSET_BYTE_LENGTH
from .exceptions import SSZError, SSZTypeError, SSZValueError, SSZSerializationError
from .base import CamelModel, StrictBaseModel
from .ssz_base import SSZType, SSZModel
from .uint import BaseUint, Uint8, Uint16, Uint32, Uint64
INITEOF
cat > "$DIR/main.py" << 'EOF'
from lean_spec.types.uint import Uint64, Uint8
a = Uint64(42)
print(int(a))
print(type(a).__name__)
b = Uint64(8)
print(int(a + b))
print(int(a - b))
data = a.encode_bytes()
print(len(data))
c = Uint64.decode_bytes(data)
print(int(c))
print(c == a)
try:
    Uint8(256)
except Exception as e:
    print(type(e).__name__)
EOF
run_test "Uint64 arithmetic + encode/decode" "$DIR" "main.py" "42
Uint64
50
34
8
42
True
SSZValueError"

# ============================================================
echo "=== Tier 3: boolean.py (Boolean) ==="

DIR=$(mktemp -d)
setup_types_dir "$DIR"
cat > "$DIR/lean_spec/types/__init__.py" << 'INITEOF'
from .constants import OFFSET_BYTE_LENGTH
from .exceptions import SSZError, SSZTypeError, SSZValueError, SSZSerializationError
from .base import CamelModel, StrictBaseModel
from .ssz_base import SSZType, SSZModel
from .boolean import Boolean
INITEOF
cat > "$DIR/main.py" << 'EOF'
from lean_spec.types.boolean import Boolean
b = Boolean(True)
print(int(b))
data = b.encode_bytes()
print(len(data))
b2 = Boolean.decode_bytes(data)
print(b2 == b)
EOF
run_test "Boolean encode/decode" "$DIR" "main.py" "1
1
True"

# ============================================================
echo "=== Tier 4: container.py (Container serialize/deserialize) ==="

DIR=$(mktemp -d)
setup_types_dir "$DIR"
cat > "$DIR/lean_spec/types/__init__.py" << 'INITEOF'
from .constants import OFFSET_BYTE_LENGTH
from .exceptions import SSZError, SSZTypeError, SSZValueError, SSZSerializationError
from .base import CamelModel, StrictBaseModel
from .ssz_base import SSZType, SSZModel
from .uint import BaseUint, Uint8, Uint16, Uint32, Uint64
from .boolean import Boolean
from .container import Container
INITEOF
cat > "$DIR/main.py" << 'EOF'
from lean_spec.types.uint import Uint64
from lean_spec.types.boolean import Boolean
from lean_spec.types.container import Container

class Checkpoint(Container):
    epoch: Uint64 = Uint64(0)
    active: Boolean = Boolean(False)

cp = Checkpoint(epoch=Uint64(42), active=Boolean(True))
print(int(cp.epoch))
print(int(cp.active))
print(Checkpoint.is_fixed_size())
print(Checkpoint.get_byte_length())
data = cp.encode_bytes()
print(len(data))
cp2 = Checkpoint.decode_bytes(data)
print(int(cp2.epoch))
print(int(cp2.active))
print(cp2 == cp)
EOF
run_test "Container serialize/deserialize round-trip" "$DIR" "main.py" "42
1
True
9
9
42
1
True"

# ============================================================
echo "=== Tier 5: byte_arrays.py (BaseBytes, Bytes32, Bytes4) ==="

DIR=$(mktemp -d)
setup_types_dir_tier5 "$DIR"
cat > "$DIR/lean_spec/types/__init__.py" << 'INITEOF'
from .constants import OFFSET_BYTE_LENGTH
from .exceptions import SSZError, SSZTypeError, SSZValueError, SSZSerializationError
from .base import CamelModel, StrictBaseModel
from .ssz_base import SSZType, SSZModel
from .uint import BaseUint, Uint8, Uint16, Uint32, Uint64
from .boolean import Boolean
from .container import Container
from .byte_arrays import BaseBytes, Bytes4, Bytes32, ZERO_HASH
INITEOF
cat > "$DIR/main.py" << 'EOF'
from lean_spec.types.byte_arrays import BaseBytes, Bytes4, Bytes32, ZERO_HASH

# Bytes4 from raw bytes
b4 = Bytes4(b'\x01\x02\x03\x04')
print(len(b4))
print(b4.hex())

# Bytes4 from hex string
b4h = Bytes4('01020304')
print(b4h == b4)

# Bytes32.zero()
z32 = Bytes32.zero()
print(len(z32))
print(z32 == ZERO_HASH)

# is_fixed_size / get_byte_length
print(Bytes32.is_fixed_size())
print(Bytes32.get_byte_length())

# encode_bytes / decode_bytes round-trip
data = b4.encode_bytes()
b4_2 = Bytes4.decode_bytes(data)
print(b4_2 == b4)

# Wrong length raises error
try:
    Bytes4(b'\x01\x02')
except Exception as e:
    print(type(e).__name__)
EOF
run_test "byte_arrays.py BaseBytes" "$DIR" "main.py" "4
01020304
True
32
True
True
32
True
SSZValueError"

# ============================================================
echo "=== Tier 5: bitfields.py (BaseBitvector, BaseBitlist) ==="

DIR=$(mktemp -d)
setup_types_dir_tier5 "$DIR"
cat > "$DIR/lean_spec/types/__init__.py" << 'INITEOF'
from .constants import OFFSET_BYTE_LENGTH
from .exceptions import SSZError, SSZTypeError, SSZValueError, SSZSerializationError
from .base import CamelModel, StrictBaseModel
from .ssz_base import SSZType, SSZModel
from .uint import BaseUint, Uint8, Uint16, Uint32, Uint64
from .boolean import Boolean
from .container import Container
from .byte_arrays import BaseBytes, Bytes4, Bytes32, ZERO_HASH
from .bitfields import BaseBitvector, BaseBitlist
INITEOF
cat > "$DIR/main.py" << 'EOF'
from lean_spec.types.bitfields import BaseBitvector, BaseBitlist
from lean_spec.types.boolean import Boolean

class Bitvec4(BaseBitvector):
    LENGTH = 4

bv = Bitvec4(data=[Boolean(True), Boolean(False), Boolean(True), Boolean(False)])
print(len(bv.data))
print(Bitvec4.is_fixed_size())
data = bv.encode_bytes()
print(len(data))
bv2 = Bitvec4.decode_bytes(data)
print(bv2 == bv)

class Bits8(BaseBitlist):
    LIMIT = 8

bl = Bits8(data=[Boolean(True), Boolean(True), Boolean(False)])
print(len(bl.data))
print(Bits8.is_fixed_size())
data = bl.encode_bytes()
bl2 = Bits8.decode_bytes(data)
print(bl2 == bl)
EOF
run_test "bitfields.py Bitvector+Bitlist" "$DIR" "main.py" "4
True
1
True
3
False
True"

# ============================================================
echo "=== Tier 6: collections.py (SSZVector, SSZList) ==="

DIR=$(mktemp -d)
setup_types_dir_tier6 "$DIR"
cat > "$DIR/lean_spec/types/__init__.py" << 'INITEOF'
from .constants import OFFSET_BYTE_LENGTH
from .exceptions import SSZError, SSZTypeError, SSZValueError, SSZSerializationError
from .base import CamelModel, StrictBaseModel
from .ssz_base import SSZType, SSZModel
from .uint import BaseUint, Uint8, Uint16, Uint32, Uint64
from .boolean import Boolean
from .container import Container
from .byte_arrays import BaseBytes, Bytes4, Bytes32, ZERO_HASH
from .bitfields import BaseBitvector, BaseBitlist
from .collections import SSZVector, SSZList
INITEOF
cat > "$DIR/main.py" << 'EOF'
from lean_spec.types.collections import SSZVector, SSZList
from lean_spec.types.uint import Uint64

class Vec3(SSZVector):
    ELEMENT_TYPE = Uint64
    LENGTH = 3

v = Vec3(data=[Uint64(1), Uint64(2), Uint64(3)])
print(len(v))
print(Vec3.is_fixed_size())
print(Vec3.get_byte_length())
data = v.encode_bytes()
print(len(data))
v2 = Vec3.decode_bytes(data)
print(v2 == v)

class List5(SSZList):
    ELEMENT_TYPE = Uint64
    LIMIT = 5

l = List5(data=[Uint64(10), Uint64(20)])
print(len(l))
print(List5.is_fixed_size())
data = l.encode_bytes()
print(len(data))
l2 = List5.decode_bytes(data)
print(l2 == l)
EOF
run_test "collections.py SSZVector+SSZList" "$DIR" "main.py" "3
True
24
24
True
2
False
16
True"

# ============================================================
echo "=== Tier 6: union.py (SSZUnion) ==="

DIR=$(mktemp -d)
setup_types_dir_tier6 "$DIR"
cat > "$DIR/lean_spec/types/__init__.py" << 'INITEOF'
from .constants import OFFSET_BYTE_LENGTH
from .exceptions import SSZError, SSZTypeError, SSZValueError, SSZSerializationError
from .base import CamelModel, StrictBaseModel
from .ssz_base import SSZType, SSZModel
from .uint import BaseUint, Uint8, Uint16, Uint32, Uint64
from .boolean import Boolean
from .container import Container
from .byte_arrays import BaseBytes, Bytes4, Bytes32, ZERO_HASH
from .bitfields import BaseBitvector, BaseBitlist
from .collections import SSZVector, SSZList
from .union import SSZUnion
INITEOF
cat > "$DIR/main.py" << 'EOF'
from lean_spec.types.union import SSZUnion
from lean_spec.types.uint import Uint64
from lean_spec.types.boolean import Boolean

class MyUnion(SSZUnion):
    OPTIONS = (None, Uint64, Boolean)

# None variant
u0 = MyUnion(selector=0, value=None)
print(u0.selector)
print(u0.value)

# Uint64 variant
u1 = MyUnion(selector=1, value=Uint64(42))
print(u1.selector)
print(int(u1.value))

# Boolean variant
u2 = MyUnion(selector=2, value=Boolean(True))
print(u2.selector)
print(int(u2.value))

# is_fixed_size
print(MyUnion.is_fixed_size())

# Serialize/deserialize round-trip for Uint64 variant
data = u1.encode_bytes()
print(len(data))
u1_copy = MyUnion.decode_bytes(data)
print(u1_copy.selector)
print(int(u1_copy.value))
EOF
run_test "union.py SSZUnion" "$DIR" "main.py" "0
None
1
42
2
1
False
9
1
42"

# ============================================================
echo ""
echo "Results: $PASS passed, $FAIL failed"
[ $FAIL -eq 0 ] && exit 0 || exit 1
