import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Stdlib.Struct

open LeanPython.Runtime
open LeanPython.Interpreter

-- ============================================================
-- Format string parsing
-- ============================================================

/-- A single format code from a struct format string. -/
inductive FormatCode where
  | uint8                     -- B
  | int8                      -- b
  | uint16                    -- H
  | int16                     -- h
  | uint32                    -- I / L
  | int32                     -- i / l
  | uint64                    -- Q
  | int64                     -- q
  | bool_                     -- ?
  | bytes_ : Nat → FormatCode -- Ns
  | pad : Nat → FormatCode    -- Nx

/-- Size in bytes of a format code. -/
private def formatCodeSize : FormatCode → Nat
  | .uint8 | .int8 | .bool_ => 1
  | .uint16 | .int16 => 2
  | .uint32 | .int32 => 4
  | .uint64 | .int64 => 8
  | .bytes_ n | .pad n => n

/-- Consume leading digits from a character list. Returns (number, remaining). -/
private def consumeDigits (chars : List Char) : Nat × List Char :=
  go chars 0
where
  go : List Char → Nat → Nat × List Char
    | c :: rest, n =>
      if c.isDigit then go rest (n * 10 + (c.toNat - '0'.toNat))
      else (n, c :: rest)
    | [], n => (n, [])

/-- Parse format codes from character list. -/
private partial def parseCodes (bigEndian : Bool) (chars : List Char) (acc : List FormatCode)
    : Except String (Bool × List FormatCode) :=
  match chars with
  | [] => .ok (bigEndian, acc.reverse)
  | _ =>
    let (count, remaining) := consumeDigits chars
    let n := if count == 0 then 1 else count
    match remaining with
    | [] =>
      if count > 0 then .error "incomplete format: trailing count"
      else .ok (bigEndian, acc.reverse)
    | c' :: cs' =>
      let code? := match c' with
        | 'B' => some FormatCode.uint8
        | 'b' => some FormatCode.int8
        | 'H' => some FormatCode.uint16
        | 'h' => some FormatCode.int16
        | 'I' => some FormatCode.uint32
        | 'i' => some FormatCode.int32
        | 'L' => some FormatCode.uint32
        | 'l' => some FormatCode.int32
        | 'Q' => some FormatCode.uint64
        | 'q' => some FormatCode.int64
        | '?' => some FormatCode.bool_
        | 's' => some (FormatCode.bytes_ n)
        | 'x' => some (FormatCode.pad n)
        | _ => none
      match code? with
      | none => .error s!"unsupported format character '{c'}'"
      | some code =>
        match c' with
        | 's' | 'x' => parseCodes bigEndian cs' (code :: acc)
        | _ => parseCodes bigEndian cs' ((List.replicate n code) ++ acc)

/-- Parse a struct format string into endianness and list of codes. -/
private def parseFormat (fmt : String) : Except String (Bool × List FormatCode) :=
  let chars := fmt.toList
  let (bigEndian, rest) := match chars with
    | '<' :: cs => (false, cs)
    | '>' :: cs => (true, cs)
    | '!' :: cs => (true, cs)
    | '@' :: cs => (false, cs)
    | '=' :: cs => (false, cs)
    | cs => (false, cs)
  parseCodes bigEndian rest []

-- ============================================================
-- Byte packing/unpacking helpers
-- ============================================================

/-- Pack a natural number into `size` bytes with given endianness. -/
private partial def packUint (n : Nat) (size : Nat) (bigEndian : Bool) : ByteArray := Id.run do
  let mut ba := ByteArray.empty
  if bigEndian then
    for i in [:size] do
      let shift := (size - 1 - i) * 8
      ba := ba.push ((n >>> shift) % 256).toUInt8
  else
    for i in [:size] do
      let shift := i * 8
      ba := ba.push ((n >>> shift) % 256).toUInt8
  return ba

/-- Pack a signed integer into `size` bytes (two's complement) with given endianness. -/
private def packInt (n : Int) (size : Nat) (bigEndian : Bool) : ByteArray :=
  let maxVal := 2 ^ (size * 8)
  let unsigned := if n >= 0 then n.toNat else (maxVal - n.natAbs)
  packUint unsigned size bigEndian

/-- Unpack a natural number from `size` bytes at `offset` with given endianness. -/
private partial def unpackUint (b : ByteArray) (offset : Nat) (size : Nat) (bigEndian : Bool) : Nat := Id.run do
  let mut result : Nat := 0
  if bigEndian then
    for i in [:size] do
      result := result * 256 + (b.get! (offset + i)).toNat
  else
    for i in [:size] do
      let shift := i * 8
      result := result + (b.get! (offset + i)).toNat * (2 ^ shift)
  return result

/-- Unpack a signed integer from `size` bytes at `offset` (two's complement). -/
private def unpackInt (b : ByteArray) (offset : Nat) (size : Nat) (bigEndian : Bool) : Int :=
  let unsigned := unpackUint b offset size bigEndian
  let maxVal := 2 ^ (size * 8)
  let signBit := 2 ^ (size * 8 - 1)
  if unsigned >= signBit then
    Int.ofNat unsigned - Int.ofNat maxVal
  else
    Int.ofNat unsigned

-- ============================================================
-- struct.pack
-- ============================================================

/-- Python struct.pack(format, v1, v2, ...) -/
partial def structPack (args : List Value) : InterpM Value := do
  match args with
  | .str fmt :: values => do
    match parseFormat fmt with
    | .error e => throwValueError s!"struct.error: {e}"
    | .ok (bigEndian, codes) =>
      let mut result := ByteArray.empty
      let mut valIdx := 0
      for code in codes do
        match code with
        | .pad n =>
          for _ in [:n] do result := result.push 0
        | .bytes_ n =>
          if valIdx >= values.length then
            throwValueError "struct.pack requires more arguments"
          match values[valIdx]! with
          | .bytes b =>
            let len := min n b.size
            for i in [:len] do result := result.push (b.get! i)
            for _ in [:n - len] do result := result.push 0
            valIdx := valIdx + 1
          | _ => throwValueError "struct.pack: 's' format requires bytes"
        | .bool_ =>
          if valIdx >= values.length then
            throwValueError "struct.pack requires more arguments"
          let b := match values[valIdx]! with
            | .bool true => 1
            | .bool false => 0
            | .int n => if n != 0 then 1 else 0
            | _ => 0
          result := result.push (UInt8.ofNat b)
          valIdx := valIdx + 1
        | _ =>
          if valIdx >= values.length then
            throwValueError "struct.pack requires more arguments"
          let n : Int ← match values[valIdx]! with
            | .int n => pure n
            | .bool b => pure (if b then 1 else 0)
            | _ => throwValueError "struct.pack: argument must be an integer"
          let size := formatCodeSize code
          let packed := match code with
            | .int8 | .int16 | .int32 | .int64 => packInt n size bigEndian
            | _ => packUint n.toNat size bigEndian
          for i in [:packed.size] do result := result.push (packed.get! i)
          valIdx := valIdx + 1
      return .bytes result
  | _ => throwTypeError "struct.pack() requires a format string as first argument"

-- ============================================================
-- struct.unpack
-- ============================================================

/-- Python struct.unpack(format, buffer) -/
partial def structUnpack (args : List Value) : InterpM Value := do
  match args with
  | [.str fmt, .bytes buf] => do
    match parseFormat fmt with
    | .error e => throwValueError s!"struct.error: {e}"
    | .ok (bigEndian, codes) =>
      let mut results : Array Value := #[]
      let mut offset := 0
      for code in codes do
        let size := formatCodeSize code
        if offset + size > buf.size then
          throwValueError "struct.unpack: buffer too short"
        match code with
        | .pad n => offset := offset + n
        | .bytes_ n =>
          let mut ba := ByteArray.empty
          for i in [:n] do ba := ba.push (buf.get! (offset + i))
          results := results.push (.bytes ba)
          offset := offset + n
        | .bool_ =>
          results := results.push (.bool ((buf.get! offset).toNat != 0))
          offset := offset + 1
        | .int8 | .int16 | .int32 | .int64 =>
          let v := unpackInt buf offset size bigEndian
          results := results.push (.int v)
          offset := offset + size
        | _ =>
          let v := unpackUint buf offset size bigEndian
          results := results.push (.int v)
          offset := offset + size
      return .tuple results
  | _ => throwTypeError "struct.unpack() requires a format string and a bytes argument"

-- ============================================================
-- struct.calcsize
-- ============================================================

/-- Python struct.calcsize(format) -/
partial def structCalcsize (args : List Value) : InterpM Value := do
  match args with
  | [.str fmt] =>
    match parseFormat fmt with
    | .error e => throwValueError s!"struct.error: {e}"
    | .ok (_, codes) =>
      let size := codes.foldl (fun acc c => acc + formatCodeSize c) 0
      return .int size
  | _ => throwTypeError "struct.calcsize() requires a format string"

end LeanPython.Stdlib.Struct
