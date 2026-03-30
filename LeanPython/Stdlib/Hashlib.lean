import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Stdlib.Hashlib

open LeanPython.Runtime
open LeanPython.Interpreter

-- ============================================================
-- SHA-256 constants (FIPS 180-4)
-- ============================================================

/-- 64 round constants: first 32 bits of the fractional parts of the
    cube roots of the first 64 primes. -/
private def sha256K : Array UInt32 := #[
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
  0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
  0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
  0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
  0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
  0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
  0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
  0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
  0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
]

/-- Initial hash values: first 32 bits of the fractional parts of the
    square roots of the first 8 primes. -/
private def sha256H0 : Array UInt32 := #[
  0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
  0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
]

-- ============================================================
-- SHA-256 helper functions
-- ============================================================

private def rotr (x : UInt32) (n : UInt32) : UInt32 :=
  (x >>> n) ||| (x <<< (32 - n))

private def ch (x y z : UInt32) : UInt32 :=
  (x &&& y) ^^^ (x.complement &&& z)

private def maj (x y z : UInt32) : UInt32 :=
  (x &&& y) ^^^ (x &&& z) ^^^ (y &&& z)

private def bigSigma0 (x : UInt32) : UInt32 :=
  rotr x 2 ^^^ rotr x 13 ^^^ rotr x 22

private def bigSigma1 (x : UInt32) : UInt32 :=
  rotr x 6 ^^^ rotr x 11 ^^^ rotr x 25

private def smallSigma0 (x : UInt32) : UInt32 :=
  rotr x 7 ^^^ rotr x 18 ^^^ (x >>> 3)

private def smallSigma1 (x : UInt32) : UInt32 :=
  rotr x 17 ^^^ rotr x 19 ^^^ (x >>> 10)

-- ============================================================
-- SHA-256 padding
-- ============================================================

/-- Pad message per FIPS 180-4: append 0x80, zero-pad to 56 mod 64,
    append 64-bit big-endian bit length. -/
private partial def sha256Pad (msg : ByteArray) : ByteArray := Id.run do
  let bitLen := msg.size * 8
  let mut padded := msg
  padded := padded.push 0x80
  while padded.size % 64 != 56 do
    padded := padded.push 0x00
  -- Append 64-bit big-endian length
  for i in [:8] do
    padded := padded.push (UInt8.ofNat ((bitLen >>> (56 - i * 8)) % 256))
  padded

-- ============================================================
-- SHA-256 block processing
-- ============================================================

/-- Read a big-endian UInt32 from a ByteArray at the given offset. -/
private def getUInt32BE (b : ByteArray) (offset : Nat) : UInt32 :=
  let b0 := (b.get! offset).toUInt32
  let b1 := (b.get! (offset + 1)).toUInt32
  let b2 := (b.get! (offset + 2)).toUInt32
  let b3 := (b.get! (offset + 3)).toUInt32
  (b0 <<< 24) ||| (b1 <<< 16) ||| (b2 <<< 8) ||| b3

/-- Process one 64-byte block, returning updated hash state. -/
private partial def processBlock (h : Array UInt32) (block : ByteArray) (offset : Nat)
    : Array UInt32 := Id.run do
  -- Prepare message schedule W[0..63]
  let mut w : Array UInt32 := Array.replicate 64 0
  for t in [:16] do
    w := w.set! t (getUInt32BE block (offset + t * 4))
  for t in [16:64] do
    w := w.set! t (smallSigma1 w[t - 2]! + w[t - 7]! +
                    smallSigma0 w[t - 15]! + w[t - 16]!)
  -- Initialize working variables
  let mut a := h[0]!
  let mut b := h[1]!
  let mut c := h[2]!
  let mut d := h[3]!
  let mut e := h[4]!
  let mut f := h[5]!
  let mut g := h[6]!
  let mut hh := h[7]!
  -- 64 rounds
  for t in [:64] do
    let t1 := hh + bigSigma1 e + ch e f g + sha256K[t]! + w[t]!
    let t2 := bigSigma0 a + maj a b c
    hh := g
    g := f
    f := e
    e := d + t1
    d := c
    c := b
    b := a
    a := t1 + t2
  -- Add compressed chunk to hash value
  #[h[0]! + a, h[1]! + b, h[2]! + c, h[3]! + d,
    h[4]! + e, h[5]! + f, h[6]! + g, h[7]! + hh]

-- ============================================================
-- SHA-256 top-level
-- ============================================================

/-- Compute the SHA-256 digest of a message (returns 32 bytes). -/
partial def sha256 (msg : ByteArray) : ByteArray := Id.run do
  let padded := sha256Pad msg
  let numBlocks := padded.size / 64
  let mut h := sha256H0
  for i in [:numBlocks] do
    h := processBlock h padded (i * 64)
  -- Serialize hash state to 32 bytes (big-endian)
  let mut result := ByteArray.empty
  for i in [:8] do
    let word := h[i]!
    result := result.push (UInt8.ofNat ((word >>> 24).toNat % 256))
    result := result.push (UInt8.ofNat ((word >>> 16).toNat % 256))
    result := result.push (UInt8.ofNat ((word >>> 8).toNat % 256))
    result := result.push (UInt8.ofNat (word.toNat % 256))
  result

-- ============================================================
-- Keccak / SHAKE-128 (FIPS 202)
-- ============================================================

/-- Keccak round constants (24 rounds). -/
private def keccakRC : Array UInt64 := #[
  0x0000000000000001, 0x0000000000008082, 0x800000000000808A,
  0x8000000080008000, 0x000000000000808B, 0x0000000080000001,
  0x8000000080008081, 0x8000000000008009, 0x000000000000008A,
  0x0000000000000088, 0x0000000080008009, 0x000000008000000A,
  0x000000008000808B, 0x800000000000008B, 0x8000000000008089,
  0x8000000000008003, 0x8000000000008002, 0x8000000000000080,
  0x000000000000800A, 0x800000008000000A, 0x8000000080008081,
  0x8000000000008080, 0x0000000080000001, 0x8000000080008008
]

/-- Keccak rotation offsets for rho step, indexed by (x + 5*y). -/
private def keccakRotations : Array UInt64 := #[
   0,  1, 62, 28, 27,
  36, 44,  6, 55, 20,
   3, 10, 43, 25, 39,
  41, 45, 15, 21,  8,
  18,  2, 61, 56, 14
]

private def rotl64 (x : UInt64) (n : UInt64) : UInt64 :=
  if n == 0 then x
  else (x <<< n) ||| (x >>> (64 - n))

/-- Run the Keccak-f[1600] permutation on a 25-word state. -/
private partial def keccakF (state : Array UInt64) : Array UInt64 := Id.run do
  let mut st := state
  for round in [:24] do
    -- Theta step
    let mut c : Array UInt64 := Array.replicate 5 0
    for x in [:5] do
      c := c.set! x (st[x]! ^^^ st[x + 5]! ^^^ st[x + 10]! ^^^
                      st[x + 15]! ^^^ st[x + 20]!)
    let mut d : Array UInt64 := Array.replicate 5 0
    for x in [:5] do
      d := d.set! x (c[(x + 4) % 5]! ^^^ rotl64 c[(x + 1) % 5]! 1)
    for x in [:5] do
      for y in [:5] do
        st := st.set! (x + 5 * y) (st[x + 5 * y]! ^^^ d[x]!)
    -- Rho and Pi steps (combined)
    let mut temp : Array UInt64 := Array.replicate 25 0
    for x in [:5] do
      for y in [:5] do
        let newX := y
        let newY := (2 * x + 3 * y) % 5
        temp := temp.set! (newX + 5 * newY)
          (rotl64 st[x + 5 * y]! keccakRotations[x + 5 * y]!)
    st := temp
    -- Chi step
    let mut chi : Array UInt64 := Array.replicate 25 0
    for x in [:5] do
      for y in [:5] do
        chi := chi.set! (x + 5 * y)
          (st[x + 5 * y]! ^^^ (st[(x + 1) % 5 + 5 * y]!.complement &&&
                                st[(x + 2) % 5 + 5 * y]!))
    st := chi
    -- Iota step
    st := st.set! 0 (st[0]! ^^^ keccakRC[round]!)
  st

/-- Convert state bytes (little-endian) to UInt64 array for Keccak. -/
private def bytesToLanes (b : ByteArray) (nLanes : Nat) : Array UInt64 := Id.run do
  let mut lanes : Array UInt64 := Array.replicate nLanes 0
  for i in [:nLanes] do
    let mut val : UInt64 := 0
    for j in [:8] do
      let byteIdx := i * 8 + j
      if byteIdx < b.size then
        val := val ||| ((b.get! byteIdx).toUInt64 <<< (j * 8).toUInt64)
    lanes := lanes.set! i val
  lanes

/-- Extract bytes from UInt64 lane (little-endian). -/
private def laneToBytes (lane : UInt64) : ByteArray := Id.run do
  let mut result := ByteArray.empty
  for j in [:8] do
    result := result.push (UInt8.ofNat ((lane >>> (j * 8).toUInt64).toNat % 256))
  result

/-- SHAKE-128: variable-length output hash using Keccak sponge.
    Rate = 168 bytes, capacity = 32 bytes, domain separator = 0x1F. -/
partial def shake128 (msg : ByteArray) (outputLen : Nat) : ByteArray := Id.run do
  let rate := 168
  -- Initialize state (25 UInt64 = 200 bytes)
  let mut state : Array UInt64 := Array.replicate 25 0
  -- Absorb phase: process full blocks
  let mut offset := 0
  while offset + rate <= msg.size do
    let blockLanes := bytesToLanes (msg.extract offset (offset + rate)) (rate / 8)
    for i in [:(rate / 8)] do
      state := state.set! i (state[i]! ^^^ blockLanes[i]!)
    state := keccakF state
    offset := offset + rate
  -- Absorb final partial block with padding
  let mut lastBlock := ByteArray.empty
  for i in [offset:msg.size] do
    lastBlock := lastBlock.push (msg.get! i)
  -- Pad: append domain separator 0x1F, then zero-pad, then set last byte's high bit
  lastBlock := lastBlock.push 0x1F
  while lastBlock.size < rate do
    lastBlock := lastBlock.push 0x00
  -- Set the last bit of the rate (0x80 on the last byte)
  let lastIdx := lastBlock.size - 1
  lastBlock := lastBlock.set! lastIdx ((lastBlock.get! lastIdx) ||| 0x80)
  let blockLanes := bytesToLanes lastBlock (rate / 8)
  for i in [:(rate / 8)] do
    state := state.set! i (state[i]! ^^^ blockLanes[i]!)
  state := keccakF state
  -- Squeeze phase
  let mut output := ByteArray.empty
  while output.size < outputLen do
    -- Extract rate bytes from state
    for i in [:(rate / 8)] do
      if output.size >= outputLen then break
      let bytes := laneToBytes state[i]!
      for j in [:8] do
        if output.size < outputLen then
          output := output.push (bytes.get! j)
    if output.size < outputLen then
      state := keccakF state
  output

-- ============================================================
-- Hex encoding helper
-- ============================================================

private def hexChar (n : Nat) : Char :=
  if n < 10 then Char.ofNat (n + '0'.toNat)
  else Char.ofNat (n - 10 + 'a'.toNat)

/-- Convert a ByteArray to a lowercase hex string. -/
def bytesToHex (b : ByteArray) : String := Id.run do
  let mut result : String := ""
  for i in [:b.size] do
    let byte := (b.get! i).toNat
    result := result.push (hexChar (byte / 16))
    result := result.push (hexChar (byte % 16))
  result

-- ============================================================
-- Hash object method dispatch
-- ============================================================

/-- Dispatch a method call on a hash object instance (SHA256Hash or SHAKE128Hash).
    The instance stores `_buffer` (bytes), `_name` (str), `_is_shake` (bool). -/
partial def callHashMethod (iref : HeapRef) (method : String) (args : List Value)
    : InterpM Value := do
  let id_ ← heapGetInstanceData iref
  let buf := match id_.attrs["_buffer"]? with
    | some (.bytes b) => b
    | _ => ByteArray.empty
  let name := match id_.attrs["_name"]? with
    | some (.str s) => s
    | _ => "sha256"
  let isShake := match id_.attrs["_is_shake"]? with
    | some (.bool b) => b
    | _ => false
  match method with
  | "update" =>
    match args with
    | [.bytes data] =>
      let newBuf := buf ++ data
      let attrs := id_.attrs.insert "_buffer" (.bytes newBuf)
      heapSetInstanceData iref { id_ with attrs := attrs }
      return .none
    | _ => throwTypeError "hash.update() requires a bytes-like argument"
  | "digest" =>
    if isShake then
      match args with
      | [.int n] =>
        let result := shake128 buf n.toNat
        return .bytes result
      | _ => throwTypeError "shake.digest() requires a length argument"
    else
      match name with
      | "sha256" => return .bytes (sha256 buf)
      | _ => throwNotImplemented s!"hash algorithm '{name}' not implemented"
  | "hexdigest" =>
    if isShake then
      match args with
      | [.int n] =>
        let result := shake128 buf n.toNat
        return .str (bytesToHex result)
      | _ => throwTypeError "shake.hexdigest() requires a length argument"
    else
      match name with
      | "sha256" => return .str (bytesToHex (sha256 buf))
      | _ => throwNotImplemented s!"hash algorithm '{name}' not implemented"
  | "copy" =>
    let cls := id_.cls
    let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := id_.attrs })
    return .instance instRef
  | _ => throwAttributeError s!"'_hashlib.HASH' object has no attribute '{method}'"

end LeanPython.Stdlib.Hashlib
