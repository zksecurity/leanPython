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
echo "=== Tier 7: rlp.py (RLP encode/decode) ==="

DIR=$(mktemp -d)
setup_types_dir_tier6 "$DIR"
cp "$SRC/types/rlp.py" "$DIR/lean_spec/types/"
cat > "$DIR/main.py" << 'EOF'
from lean_spec.types.rlp import encode_rlp, decode_rlp, decode_rlp_list, RLPDecodingError

# Single byte encoding: 0x00-0x7f encode as themselves
assert encode_rlp(b'\x00') == b'\x00'
assert encode_rlp(b'\x7f') == b'\x7f'
print('single byte ok')

# Short string encoding: prefix 0x80 + length
data = b'hello'
enc = encode_rlp(data)
assert enc == b'\x85hello'
print('short string ok')

# Empty string -> 0x80
assert encode_rlp(b'') == b'\x80'
print('empty string ok')

# Empty list -> 0xc0
assert encode_rlp([]) == b'\xc0'
print('empty list ok')

# Round-trip: bytes
rt = decode_rlp(encode_rlp(b'hello'))
assert rt == b'hello'
print('round-trip bytes ok')

# Round-trip: flat list
original = [b'\x01', b'\x02', b'\x03']
rt = decode_rlp(encode_rlp(original))
assert rt == original
print('round-trip list ok')

# Round-trip: nested list
nested = [b'\x01', [b'\x02', b'\x03']]
enc = encode_rlp(nested)
dec = decode_rlp(enc)
assert dec == nested
print('nested list ok')

# decode_rlp_list convenience function
flat = [b'\x01', b'\x02', b'\x03']
enc = encode_rlp(flat)
dec = decode_rlp_list(enc)
assert dec == flat
print('decode_rlp_list ok')

# Error handling
try:
    decode_rlp(b'')
    assert False, "should have raised"
except RLPDecodingError:
    print('error handling ok')
EOF
run_test "rlp.py encode/decode" "$DIR" "main.py" "single byte ok
short string ok
empty string ok
empty list ok
round-trip bytes ok
round-trip list ok
nested list ok
decode_rlp_list ok
error handling ok"

# ============================================================
# Subspecs layer helper: extends tier6 with full types/__init__.py + subspecs scaffolding
setup_subspecs_koalabear() {
  local dir="$1"
  setup_types_dir_tier6 "$dir"
  cp "$SRC/types/rlp.py" "$dir/lean_spec/types/"
  # Full types/__init__.py matching real leanSpec
  cat > "$dir/lean_spec/types/__init__.py" << 'INITEOF'
from .constants import OFFSET_BYTE_LENGTH
from .exceptions import SSZError, SSZTypeError, SSZValueError, SSZSerializationError
from .base import CamelModel, StrictBaseModel
from .ssz_base import SSZType, SSZModel
from .uint import BaseUint, Uint8, Uint16, Uint32, Uint64
from .boolean import Boolean
from .container import Container
from .byte_arrays import BaseBytes, BaseByteList, Bytes4, Bytes32, Bytes52, ZERO_HASH
from .bitfields import BaseBitvector, BaseBitlist
from .collections import SSZVector, SSZList
from .union import SSZUnion
INITEOF
  # Subspecs scaffolding
  mkdir -p "$dir/lean_spec/subspecs/koalabear"
  cp "$SRC/subspecs/__init__.py" "$dir/lean_spec/subspecs/"
  cp "$SRC/subspecs/koalabear/__init__.py" "$dir/lean_spec/subspecs/koalabear/"
  cp "$SRC/subspecs/koalabear/field.py" "$dir/lean_spec/subspecs/koalabear/"
}

setup_subspecs_ssz_base() {
  local dir="$1"
  setup_subspecs_koalabear "$dir"
  mkdir -p "$dir/lean_spec/subspecs/ssz"
  # Shimmed __init__.py (real one imports hash.py which needs full chain)
  echo '"""SSZ (Simple Serialize) implementation."""' > "$dir/lean_spec/subspecs/ssz/__init__.py"
  cp "$SRC/subspecs/ssz/constants.py" "$dir/lean_spec/subspecs/ssz/"
  cp "$SRC/subspecs/ssz/utils.py" "$dir/lean_spec/subspecs/ssz/"
}

setup_subspecs_ssz_full() {
  local dir="$1"
  setup_subspecs_ssz_base "$dir"
  cp "$SRC/subspecs/ssz/pack.py" "$dir/lean_spec/subspecs/ssz/"
  cp "$SRC/subspecs/ssz/merkleization.py" "$dir/lean_spec/subspecs/ssz/"
}

setup_subspecs_ssz_hash() {
  local dir="$1"
  setup_subspecs_ssz_full "$dir"
  cp "$SRC/subspecs/ssz/hash.py" "$dir/lean_spec/subspecs/ssz/"
  # Real __init__.py now that hash.py is available
  cp "$SRC/subspecs/ssz/__init__.py" "$dir/lean_spec/subspecs/ssz/"
}

setup_subspecs_containers_basic() {
  local dir="$1"
  setup_subspecs_ssz_hash "$dir"
  # chain/ with shimmed __init__.py (real one imports clock.py which uses asyncio)
  mkdir -p "$dir/lean_spec/subspecs/chain"
  cat > "$dir/lean_spec/subspecs/chain/__init__.py" << 'INITEOF'
"""Specifications for chain and consensus parameters."""
from .config import (
    INTERVALS_PER_SLOT,
    SECONDS_PER_SLOT,
    MILLISECONDS_PER_SLOT,
    MILLISECONDS_PER_INTERVAL,
    HISTORICAL_ROOTS_LIMIT,
    VALIDATOR_REGISTRY_LIMIT,
)
INITEOF
  cp "$SRC/subspecs/chain/config.py" "$dir/lean_spec/subspecs/chain/"
  # containers/ with shimmed __init__.py (real one imports xmss-dependent files)
  mkdir -p "$dir/lean_spec/subspecs/containers"
  cat > "$dir/lean_spec/subspecs/containers/__init__.py" << 'INITEOF'
"""Container types for the Lean consensus specification."""
from .checkpoint import Checkpoint
from .config import Config
from .slot import Slot
INITEOF
  cp "$SRC/subspecs/containers/slot.py" "$dir/lean_spec/subspecs/containers/"
  cp "$SRC/subspecs/containers/checkpoint.py" "$dir/lean_spec/subspecs/containers/"
  cp "$SRC/subspecs/containers/config.py" "$dir/lean_spec/subspecs/containers/"
}

# ============================================================
echo "=== Tier 8: koalabear/field.py (Fp field arithmetic) ==="

DIR=$(mktemp -d)
setup_subspecs_koalabear "$DIR"
cat > "$DIR/main.py" << 'EOF'
from lean_spec.subspecs.koalabear import Fp, P

# Basic creation
a = Fp(value=7)
b = Fp(value=11)
print(a.value)
print(b.value)

# Arithmetic
c = a + b
print(c.value)
c = a - b
print(c.value)   # (7 - 11) % P
c = a * b
print(c.value)
c = -a
print(c.value)   # (-7) % P

# Exponentiation
c = a ** 3
print(c.value)   # 7^3 = 343

# Inverse and division
inv = a.inverse()
check = a * inv
print(check.value)   # should be 1

d = b / a
check = d * a
print(check.value)   # should be 11

# Equality and hash
print(Fp(value=7) == Fp(value=7))
print(Fp(value=7) == Fp(value=8))

# Encode/decode round-trip
import io
data = a.encode_bytes()
print(len(data))
a2 = Fp.decode_bytes(data)
print(a2.value)
print(a2 == a)

# two_adic_generator
g = Fp.two_adic_generator(0)
print(g.value)   # should be 1

# TypeError on non-int
try:
    Fp(value=3.14)
except TypeError:
    print('type error ok')

# Modular reduction
big = Fp(value=P + 42)
print(big.value)   # should be 42

# Negative normalization
neg = Fp(value=-1)
print(neg.value)   # should be P - 1
EOF
run_test "koalabear Fp arithmetic" "$DIR" "main.py" "7
11
18
$(python3 -c "P=2**31-2**24+1; print((7-11)%P)")
77
$(python3 -c "P=2**31-2**24+1; print((-7)%P)")
343
1
11
True
False
4
7
True
1
type error ok
42
$(python3 -c "P=2**31-2**24+1; print(P-1)")"

# ============================================================
echo "=== Tier 9: ssz/constants.py + ssz/utils.py ==="

DIR=$(mktemp -d)
setup_subspecs_ssz_base "$DIR"
cat > "$DIR/main.py" << 'EOF'
from lean_spec.subspecs.ssz.constants import BYTES_PER_CHUNK, BITS_PER_BYTE, BITS_PER_CHUNK
print(BYTES_PER_CHUNK)
print(BITS_PER_BYTE)
print(BITS_PER_CHUNK)

from lean_spec.subspecs.ssz.utils import get_power_of_two_ceil, hash_nodes
# Power of two ceil
for x in [0, 1, 2, 3, 4, 31, 32, 33]:
    print(get_power_of_two_ceil(x))

# hash_nodes: SHA-256 of 64 zero bytes
from lean_spec.types import Bytes32, ZERO_HASH
h = hash_nodes(ZERO_HASH, ZERO_HASH)
print(h.hex())
EOF
run_test "ssz constants + utils" "$DIR" "main.py" "32
8
256
1
1
2
4
4
32
32
64
$(python3 -c "import hashlib; print(hashlib.sha256(b'\x00'*64).hexdigest())")"

# ============================================================
echo "=== Tier 10: ssz/pack.py + ssz/merkleization.py ==="

DIR=$(mktemp -d)
setup_subspecs_ssz_full "$DIR"
cat > "$DIR/main.py" << 'EOF'
from lean_spec.subspecs.ssz.pack import pack_bytes, pack_bits
from lean_spec.subspecs.ssz.merkleization import merkleize, mix_in_length
from lean_spec.types import ZERO_HASH, Bytes32

# pack_bytes: 8 bytes -> 1 chunk (padded to 32)
chunks = pack_bytes(b'\x01\x02\x03\x04\x05\x06\x07\x08')
print(len(chunks))

# pack_bytes: 32 bytes -> 1 chunk (exact)
chunks = pack_bytes(b'\x00' * 32)
print(len(chunks))
print(chunks[0] == ZERO_HASH)

# pack_bytes: 33 bytes -> 2 chunks
chunks = pack_bytes(b'\x00' * 33)
print(len(chunks))

# pack_bits: 8 bools
chunks = pack_bits([True, False, True, False, False, False, False, False])
print(len(chunks))
# First byte should be 0b00000101 = 5, rest zeros
print(chunks[0][0])

# merkleize: single chunk = identity
single = Bytes32(b'\x42' + b'\x00' * 31)
root = merkleize([single])
print(root[0])

# merkleize: empty = ZERO_HASH
root = merkleize([])
print(root == ZERO_HASH)

# mix_in_length
root = mix_in_length(ZERO_HASH, 0)
print(root == merkleize([ZERO_HASH, Bytes32(b'\x00' * 32)]))

# Module-level _ZERO_HASHES precompute works (import succeeded = pass)
print('merkleization ok')
EOF
run_test "ssz pack + merkleization" "$DIR" "main.py" "1
1
True
2
1
5
66
True
True
merkleization ok"

# ============================================================
echo "=== Tier 11: ssz/hash.py (hash_tree_root via singledispatch) ==="

DIR=$(mktemp -d)
setup_subspecs_ssz_hash "$DIR"
cat > "$DIR/main.py" << 'EOF'
from lean_spec.subspecs.ssz import hash_tree_root
from lean_spec.types import Uint64, Boolean, Bytes32, Container, SSZVector, SSZList, ZERO_HASH
from lean_spec.subspecs.koalabear import Fp

# hash_tree_root(Uint64)
h = hash_tree_root(Uint64(42))
print(h.hex())

# hash_tree_root(Boolean)
h = hash_tree_root(Boolean(True))
print(h.hex())

# hash_tree_root(Bytes32) -- ZERO_HASH
h = hash_tree_root(ZERO_HASH)
print(h.hex())

# hash_tree_root(Fp)
h = hash_tree_root(Fp(value=7))
print(h.hex())

# hash_tree_root(Container)
class Point(Container):
    x: Uint64
    y: Uint64

p = Point(x=Uint64(1), y=Uint64(2))
h = hash_tree_root(p)
print(h.hex())

# hash_tree_root(SSZVector)
class Vec3(SSZVector):
    ELEMENT_TYPE = Uint64
    LENGTH = 3

v = Vec3(data=[Uint64(10), Uint64(20), Uint64(30)])
h = hash_tree_root(v)
print(h.hex())

# hash_tree_root(SSZList)
class List5(SSZList):
    ELEMENT_TYPE = Uint64
    LIMIT = 5

l = List5(data=[Uint64(100)])
h = hash_tree_root(l)
print(h.hex())

print('hash_tree_root ok')
EOF

# Generate expected hashes from CPython
EXPECTED=$(python3 << 'PYEOF'
import sys, os
sys.path.insert(0, "references/leanSpec/src")
from lean_spec.subspecs.ssz import hash_tree_root
from lean_spec.types import Uint64, Boolean, Bytes32, Container, SSZVector, SSZList, ZERO_HASH
from lean_spec.subspecs.koalabear import Fp

print(hash_tree_root(Uint64(42)).hex())
print(hash_tree_root(Boolean(True)).hex())
print(hash_tree_root(ZERO_HASH).hex())
print(hash_tree_root(Fp(value=7)).hex())

class Point(Container):
    x: Uint64
    y: Uint64
print(hash_tree_root(Point(x=Uint64(1), y=Uint64(2))).hex())

class Vec3(SSZVector):
    ELEMENT_TYPE = Uint64
    LENGTH = 3
print(hash_tree_root(Vec3(data=[Uint64(10), Uint64(20), Uint64(30)])).hex())

class List5(SSZList):
    ELEMENT_TYPE = Uint64
    LIMIT = 5
print(hash_tree_root(List5(data=[Uint64(100)])).hex())

print('hash_tree_root ok')
PYEOF
)
run_test "ssz hash_tree_root singledispatch" "$DIR" "main.py" "$EXPECTED"

# ============================================================
echo "=== Tier 12: chain/config + containers (Slot, Checkpoint, Config) ==="

DIR=$(mktemp -d)
setup_subspecs_containers_basic "$DIR"
cat > "$DIR/main.py" << 'EOF'
# chain/config constants
from lean_spec.subspecs.chain.config import (
    SECONDS_PER_SLOT, MILLISECONDS_PER_SLOT,
    VALIDATOR_REGISTRY_LIMIT, INTERVALS_PER_SLOT,
)
from lean_spec.types import Uint64
print(int(SECONDS_PER_SLOT))
print(int(MILLISECONDS_PER_SLOT))
print(int(VALIDATOR_REGISTRY_LIMIT))
print(int(INTERVALS_PER_SLOT))

# Slot
from lean_spec.subspecs.containers.slot import Slot
s = Slot(10)
print(int(s))
# is_justifiable_after: delta=1 (<=5, always justifiable)
print(s.is_justifiable_after(Slot(9)))
# is_justifiable_after: delta=9 (perfect square)
print(Slot(9).is_justifiable_after(Slot(0)))
# is_justifiable_after: delta=6 (pronic number 2*3)
print(Slot(6).is_justifiable_after(Slot(0)))
# is_justifiable_after: delta=7 (not justifiable)
print(Slot(7).is_justifiable_after(Slot(0)))
# justified_index_after
print(s.justified_index_after(Slot(7)))

# Checkpoint
from lean_spec.subspecs.containers.checkpoint import Checkpoint
from lean_spec.types import Bytes32
cp = Checkpoint(root=Bytes32.zero(), slot=Slot(42))
print(int(cp.slot))
print(cp.root == Bytes32.zero())
data = cp.encode_bytes()
print(len(data))
cp2 = Checkpoint.decode_bytes(data)
print(cp2 == cp)

# Config
from lean_spec.subspecs.containers.config import Config
cfg = Config(genesis_time=Uint64(1704085200))
print(int(cfg.genesis_time))
data = cfg.encode_bytes()
cfg2 = Config.decode_bytes(data)
print(cfg2 == cfg)
EOF
run_test "chain config + Slot + Checkpoint + Config" "$DIR" "main.py" "4
4000
4096
5
10
True
True
True
False
2
42
True
40
True
1704085200
True"


# ============================================================
# Full containers setup: adds XMSS stubs + all real container files
# ============================================================
setup_subspecs_containers_full() {
  local dir="$1"
  setup_subspecs_containers_basic "$dir"

  # --- XMSS stub modules (avoid lean_multisig_py and circular deps) ---
  mkdir -p "$dir/lean_spec/subspecs/xmss"
  echo '"""XMSS stub package."""' > "$dir/lean_spec/subspecs/xmss/__init__.py"

  cat > "$dir/lean_spec/subspecs/xmss/containers.py" << 'XMSSEOF'
"""Stub XMSS containers for testing without lean_multisig_py."""
from __future__ import annotations
from lean_spec.types import Bytes32, Uint64
from lean_spec.types.container import Container

class PublicKey(Container):
    """Stub public key."""
    root: Bytes32
    parameter: Bytes32

    @classmethod
    def decode_bytes(cls, data):
        half = len(data) // 2
        return cls(root=Bytes32(data[:32]), parameter=Bytes32(data[32:64]))

class Signature(Container):
    """Stub signature."""
    path: Bytes32
    rho: Bytes32
    hashes: Bytes32

    @classmethod
    def is_fixed_size(cls):
        return True

    @classmethod
    def get_byte_length(cls):
        return 96

    def verify(self, public_key, slot, message, scheme):
        return True

class SecretKey(Container):
    """Stub secret key."""
    data: Bytes32

class KeyPair:
    def __init__(self, public, secret):
        self.public = public
        self.secret = secret

class ValidatorKeyPair:
    def __init__(self, attestation_public=None, attestation_secret=None,
                 proposal_public=None, proposal_secret=None):
        self.attestation_public = attestation_public
        self.attestation_secret = attestation_secret
        self.proposal_public = proposal_public
        self.proposal_secret = proposal_secret
XMSSEOF

  cat > "$dir/lean_spec/subspecs/xmss/aggregation.py" << 'XMSSEOF'
"""Stub XMSS aggregation for testing without lean_multisig_py."""
from __future__ import annotations
from lean_spec.types import Bytes32
from lean_spec.types.container import Container

class AggregationError(Exception):
    """Raised when signature aggregation or verification fails."""

class AggregatedSignatureProof(Container):
    """Stub aggregated signature proof."""
    participants: Bytes32
    proof_data: Bytes32
XMSSEOF

  cat > "$dir/lean_spec/subspecs/xmss/interface.py" << 'XMSSEOF'
"""Stub XMSS interface for testing without lean_multisig_py."""
TARGET_SIGNATURE_SCHEME = None

class GeneralizedXmssScheme:
    pass
XMSSEOF

  # --- containers/validator.py (REAL file) ---
  cp "$SRC/subspecs/containers/validator.py" "$dir/lean_spec/subspecs/containers/"

  # --- containers/attestation/ (REAL files + shimmed __init__) ---
  mkdir -p "$dir/lean_spec/subspecs/containers/attestation"
  cat > "$dir/lean_spec/subspecs/containers/attestation/__init__.py" << 'INITEOF'
"""Attestation containers and related types."""
from .aggregation_bits import AggregationBits
from .attestation import (
    AggregatedAttestation,
    Attestation,
    AttestationData,
)
INITEOF
  cp "$SRC/subspecs/containers/attestation/aggregation_bits.py" \
     "$dir/lean_spec/subspecs/containers/attestation/"
  cp "$SRC/subspecs/containers/attestation/attestation.py" \
     "$dir/lean_spec/subspecs/containers/attestation/"

  # --- containers/block/ (REAL block.py + modified types.py + shimmed __init__) ---
  mkdir -p "$dir/lean_spec/subspecs/containers/block"
  cat > "$dir/lean_spec/subspecs/containers/block/__init__.py" << 'INITEOF'
"""Block containers (without BlockLookup/SignedBlock which need dict inheritance/deep XMSS)."""
from .block import Block, BlockBody, BlockHeader
from .types import AggregatedAttestations, AttestationSignatures
INITEOF
  cp "$SRC/subspecs/containers/block/block.py" "$dir/lean_spec/subspecs/containers/block/"
  # Modified block/types.py: skip BlockLookup (needs dict subclass inheritance)
  cat > "$dir/lean_spec/subspecs/containers/block/types.py" << 'TYPESEOF'
"""Block-specific SSZ types (BlockLookup stubbed out — needs dict inheritance)."""
from __future__ import annotations
from lean_spec.subspecs.xmss.aggregation import AggregatedSignatureProof
from lean_spec.types import Bytes32, SSZList
from ...chain.config import VALIDATOR_REGISTRY_LIMIT
from ..attestation import AggregatedAttestation

class BlockLookup:
    """Stub — real version inherits from dict[Bytes32, Block]."""
    pass

class AggregatedAttestations(SSZList):
    """List of aggregated attestations included in a block."""
    LIMIT = int(VALIDATOR_REGISTRY_LIMIT)

class AttestationSignatures(SSZList):
    """List of per-attestation aggregated signature proofs."""
    LIMIT = int(VALIDATOR_REGISTRY_LIMIT)
TYPESEOF

  # --- containers/state/ (REAL files) ---
  mkdir -p "$dir/lean_spec/subspecs/containers/state"
  cp "$SRC/subspecs/containers/state/__init__.py" "$dir/lean_spec/subspecs/containers/state/"
  cp "$SRC/subspecs/containers/state/types.py" "$dir/lean_spec/subspecs/containers/state/"
  cp "$SRC/subspecs/containers/state/state.py" "$dir/lean_spec/subspecs/containers/state/"

  # --- Update containers/__init__.py to include all container types ---
  cat > "$dir/lean_spec/subspecs/containers/__init__.py" << 'INITEOF'
"""Container types for the Lean consensus specification."""
from .attestation import (
    AggregatedAttestation,
    AggregationBits,
    Attestation,
    AttestationData,
)
from .block import Block, BlockBody, BlockHeader
from .checkpoint import Checkpoint
from .config import Config
from .slot import Slot
from .state import State
from .validator import Validator, ValidatorIndex, ValidatorIndices
INITEOF
}

# ============================================================
echo "=== Tier 13: Full containers (Validator, Attestation, AggregationBits, State types) ==="

DIR=$(mktemp -d)
setup_subspecs_containers_full "$DIR"
cat > "$DIR/main.py" << 'EOF'
from lean_spec.types import Bytes32, Bytes52, Uint64, Boolean
from lean_spec.subspecs.containers.slot import Slot
from lean_spec.subspecs.containers.checkpoint import Checkpoint

# --- ValidatorIndex ---
from lean_spec.subspecs.containers.validator import ValidatorIndex, SubnetId, ValidatorIndices

vi = ValidatorIndex(7)
print(int(vi))
# is_proposer_for: 7 % 10 == 7 -> True
print(vi.is_proposer_for(Slot(7), Uint64(10)))
# is_proposer_for: 8 % 10 == 8 != 7 -> False
print(vi.is_proposer_for(Slot(8), Uint64(10)))
# is_valid: 7 < 10 -> True
print(vi.is_valid(Uint64(10)))
# is_valid: 7 < 5 -> False
print(vi.is_valid(Uint64(5)))
# compute_subnet_id: 7 % 4 = 3
sid = vi.compute_subnet_id(Uint64(4))
print(int(sid))

# --- AggregationBits ---
from lean_spec.subspecs.containers.attestation.aggregation_bits import AggregationBits

bits = AggregationBits.from_validator_indices(
    ValidatorIndices(data=[ValidatorIndex(0), ValidatorIndex(3)])
)
# Should create bitlist: [True, False, False, True]
print(len(bits.data))
print(bool(bits.data[0]))
print(bool(bits.data[1]))
print(bool(bits.data[3]))

# Round-trip
indices = bits.to_validator_indices()
print(len(indices.data))
print(int(indices.data[0]))
print(int(indices.data[1]))

# --- AttestationData ---
from lean_spec.subspecs.containers.attestation.attestation import (
    AttestationData, Attestation, AggregatedAttestation,
)

cp_head = Checkpoint(root=Bytes32(b'\x01' * 32), slot=Slot(5))
cp_target = Checkpoint(root=Bytes32(b'\x02' * 32), slot=Slot(10))
cp_source = Checkpoint(root=Bytes32(b'\x03' * 32), slot=Slot(1))

att_data = AttestationData(
    slot=Slot(10),
    head=cp_head,
    target=cp_target,
    source=cp_source,
)
print(int(att_data.slot))
print(att_data.head.slot == Slot(5))

# --- AggregatedAttestation.aggregate_by_data ---
atts = [
    Attestation(validator_id=ValidatorIndex(0), data=att_data),
    Attestation(validator_id=ValidatorIndex(2), data=att_data),
    Attestation(validator_id=ValidatorIndex(5), data=att_data),
]
aggs = AggregatedAttestation.aggregate_by_data(atts)
# All same att_data -> 1 aggregation
print(len(aggs))
# Aggregation bits cover indices 0, 2, 5 -> length 6
print(len(aggs[0].aggregation_bits.data))

# --- JustifiedSlots ---
from lean_spec.subspecs.containers.state.types import JustifiedSlots

js = JustifiedSlots(data=[Boolean(False), Boolean(False), Boolean(True)])
# Slot at or before finalized -> always justified
print(bool(js.is_slot_justified(Slot(0), Slot(0))))
# Slot 1 -> index 0 -> False
print(bool(js.is_slot_justified(Slot(0), Slot(1))))
# Slot 3 -> index 2 -> True
print(bool(js.is_slot_justified(Slot(0), Slot(3))))

# with_justified: set slot 1 to justified
js2 = js.with_justified(Slot(0), Slot(1), Boolean(True))
print(bool(js2.is_slot_justified(Slot(0), Slot(1))))

# extend_to_slot: extend to cover slot 6 (index 5)
js3 = js.extend_to_slot(Slot(0), Slot(6))
print(len(js3.data))

# shift_window: drop first 2 entries
js4 = js.shift_window(2)
print(len(js4.data))

# --- Validator ---
from lean_spec.subspecs.containers.validator import Validator
from lean_spec.subspecs.containers.state.types import Validators

v = Validator(
    attestation_pubkey=Bytes52(b'\xaa' * 52),
    proposal_pubkey=Bytes52(b'\xbb' * 52),
    index=ValidatorIndex(42),
)
print(int(v.index))

# Validators SSZList
vals = Validators(data=[v])
print(len(vals))
print(int(vals[0].index))
EOF
run_test "full containers (validator, attestation, aggregation, state types)" "$DIR" "main.py" "7
True
False
True
False
3
4
True
False
True
2
0
3
10
True
1
6
True
False
True
True
6
1
42
1
42"

# ============================================================
echo "=== Tier 14: State machine (generate_genesis, process_slots, process_block, state_transition) ==="

DIR=$(mktemp -d)
setup_subspecs_containers_full "$DIR"
cat > "$DIR/main.py" << 'EOF'
from lean_spec.types import Bytes32, Bytes52, Uint64, Boolean
from lean_spec.subspecs.containers.slot import Slot
from lean_spec.subspecs.containers.checkpoint import Checkpoint
from lean_spec.subspecs.containers.config import Config
from lean_spec.subspecs.containers.validator import Validator, ValidatorIndex
from lean_spec.subspecs.containers.block.block import Block, BlockBody, BlockHeader
from lean_spec.subspecs.containers.block.types import AggregatedAttestations
from lean_spec.subspecs.containers.state.types import Validators
from lean_spec.subspecs.containers.state.state import State
from lean_spec.subspecs.ssz.hash import hash_tree_root

# --- generate_genesis ---
v1 = Validator(
    attestation_pubkey=Bytes52(b'\x01' * 52),
    proposal_pubkey=Bytes52(b'\x02' * 52),
    index=ValidatorIndex(0),
)
v2 = Validator(
    attestation_pubkey=Bytes52(b'\x03' * 52),
    proposal_pubkey=Bytes52(b'\x04' * 52),
    index=ValidatorIndex(1),
)
v3 = Validator(
    attestation_pubkey=Bytes52(b'\x05' * 52),
    proposal_pubkey=Bytes52(b'\x06' * 52),
    index=ValidatorIndex(2),
)
validators = Validators(data=[v1, v2, v3])

genesis = State.generate_genesis(Uint64(1000), validators)
print(int(genesis.slot))
print(int(genesis.config.genesis_time))
print(len(genesis.validators))
print(genesis.latest_justified.root == Bytes32.zero())
print(genesis.latest_finalized.root == Bytes32.zero())

# --- process_slots: advance from 0 to 3 ---
state1 = genesis.process_slots(Slot(3))
print(int(state1.slot))
# After process_slots, latest_block_header.state_root should be filled
# (genesis header had zero state_root, so first slot caches it)
print(state1.latest_block_header.state_root != Bytes32.zero())

# --- process_block_header ---
# Build a block at slot 3
parent_root = hash_tree_root(state1.latest_block_header)
# Proposer for slot 3 with 3 validators: 3 % 3 = 0
block = Block(
    slot=Slot(3),
    proposer_index=ValidatorIndex(0),
    parent_root=parent_root,
    state_root=Bytes32.zero(),
    body=BlockBody(attestations=AggregatedAttestations(data=[])),
)
state2 = state1.process_block_header(block)
print(int(state2.slot))
# latest_block_header should now be for slot 3
print(int(state2.latest_block_header.slot))
# State root should be zero (not yet cached until next slot)
print(state2.latest_block_header.state_root == Bytes32.zero())

# --- state_transition (two-pass build) ---
# Step 1: process block with zero state_root to get post-state
state3 = genesis.process_slots(Slot(1))
parent_root2 = hash_tree_root(state3.latest_block_header)
# Proposer for slot 1 with 3 validators: 1 % 3 = 1
trial_block = Block(
    slot=Slot(1),
    proposer_index=ValidatorIndex(1),
    parent_root=parent_root2,
    state_root=Bytes32.zero(),
    body=BlockBody(attestations=AggregatedAttestations(data=[])),
)
post_state = state3.process_block(trial_block)
# Step 2: compute real state root and rebuild block
real_state_root = hash_tree_root(post_state)
final_block = trial_block.model_copy(update={"state_root": real_state_root})
# Step 3: full state_transition should succeed
result_state = genesis.state_transition(final_block)
print(int(result_state.slot))
# Verify it's the same post-state
print(hash_tree_root(result_state) == real_state_root)

# --- negative test: wrong state_root should fail ---
bad_block = trial_block.model_copy(update={"state_root": Bytes32(b'\xff' * 32)})
try:
    genesis.state_transition(bad_block)
    print("ERROR: should have raised")
except AssertionError:
    print("correctly rejected bad state root")
EOF
run_test "state machine (genesis, process_slots, process_block, state_transition)" "$DIR" "main.py" "0
1000
3
True
True
3
True
3
3
True
1
True
correctly rejected bad state root"

# ============================================================
echo ""
echo "Results: $PASS passed, $FAIL failed"
[ $FAIL -eq 0 ] && exit 0 || exit 1
