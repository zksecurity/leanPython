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
echo ""
echo "Results: $PASS passed, $FAIL failed"
[ $FAIL -eq 0 ] && exit 0 || exit 1
