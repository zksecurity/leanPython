import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Stdlib.Base64

open LeanPython.Runtime
open LeanPython.Interpreter

-- ============================================================
-- Base64 encoding/decoding
-- ============================================================

private def stdAlphabet : String := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
private def urlAlphabet : String := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

private def charToIndex (c : Char) (urlSafe : Bool) : Option Nat :=
  let alpha := if urlSafe then urlAlphabet else stdAlphabet
  alpha.toList.findIdx? (· == c) |>.map id

private partial def encodeBase64 (data : ByteArray) (urlSafe : Bool) : ByteArray := Id.run do
  let alpha := if urlSafe then urlAlphabet else stdAlphabet
  let alphaArr := alpha.toList.toArray
  let mut result := ByteArray.empty
  let mut i : Nat := 0
  while i + 2 < data.size do
    let b0 := (data.get! i).toNat
    let b1 := (data.get! (i + 1)).toNat
    let b2 := (data.get! (i + 2)).toNat
    let n := b0 * 65536 + b1 * 256 + b2
    result := result.push (alphaArr[n >>> 18 &&& 63]!).toUInt8
    result := result.push (alphaArr[n >>> 12 &&& 63]!).toUInt8
    result := result.push (alphaArr[n >>> 6 &&& 63]!).toUInt8
    result := result.push (alphaArr[n &&& 63]!).toUInt8
    i := i + 3
  let remaining := data.size - i
  if remaining == 2 then
    let b0 := (data.get! i).toNat
    let b1 := (data.get! (i + 1)).toNat
    let n := b0 * 256 + b1
    result := result.push (alphaArr[n >>> 10 &&& 63]!).toUInt8
    result := result.push (alphaArr[n >>> 4 &&& 63]!).toUInt8
    result := result.push (alphaArr[(n <<< 2) &&& 63]!).toUInt8
    result := result.push '='.toUInt8
  else if remaining == 1 then
    let b0 := (data.get! i).toNat
    result := result.push (alphaArr[b0 >>> 2]!).toUInt8
    result := result.push (alphaArr[(b0 <<< 4) &&& 63]!).toUInt8
    result := result.push '='.toUInt8
    result := result.push '='.toUInt8
  return result

private partial def decodeBase64 (data : ByteArray) (urlSafe : Bool) : Except String ByteArray := do
  -- Strip padding and whitespace
  let chars := data.toList.filter (fun b => b.toNat != '='.toNat && b.toNat != '\n'.toNat && b.toNat != '\r'.toNat)
  let mut result := ByteArray.empty
  let mut buf : Nat := 0
  let mut bits : Nat := 0
  for b in chars do
    let c := Char.ofNat b.toNat
    match charToIndex c urlSafe with
    | some idx =>
      buf := (buf <<< 6) ||| idx
      bits := bits + 6
      if bits >= 8 then
        bits := bits - 8
        result := result.push ((buf >>> bits) &&& 255).toUInt8
        buf := buf &&& ((1 <<< bits) - 1)
    | none => .error s!"Invalid character in base64: '{c}'"
  return result

-- ============================================================
-- Base16 (hex) encoding/decoding
-- ============================================================

private def hexCharsUpper : String := "0123456789ABCDEF"

private partial def encodeBase16 (data : ByteArray) : ByteArray := Id.run do
  let hexArr := hexCharsUpper.toList.toArray
  let mut result := ByteArray.empty
  for i in [:data.size] do
    let b := (data.get! i).toNat
    result := result.push (hexArr[b >>> 4]!).toUInt8
    result := result.push (hexArr[b &&& 15]!).toUInt8
  return result

private def hexCharToNibble (c : Char) : Option Nat :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat)
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
  else none

private partial def decodeBase16 (data : ByteArray) : Except String ByteArray := do
  if data.size % 2 != 0 then .error "Odd-length string"
  let mut result := ByteArray.empty
  let mut i : Nat := 0
  while i + 1 < data.size do
    let c1 := Char.ofNat (data.get! i).toNat
    let c2 := Char.ofNat (data.get! (i + 1)).toNat
    match hexCharToNibble c1, hexCharToNibble c2 with
    | some h, some l => result := result.push (h * 16 + l).toUInt8
    | _, _ => .error s!"Non-hexadecimal digit found"
    i := i + 2
  return result

-- ============================================================
-- Python-facing functions
-- ============================================================

/-- Coerce args to ByteArray for base64 functions. -/
private def toByteArray (v : Value) : InterpM ByteArray := do
  match v with
  | .bytes b => return b
  | .str s => return s.toUTF8
  | _ => throwTypeError "a bytes-like object is required"

/-- Python base64.b64encode(s) -/
partial def b64encode (args : List Value) : InterpM Value := do
  match args with
  | [v] => do
    let data ← toByteArray v
    return .bytes (encodeBase64 data false)
  | _ => throwTypeError "b64encode() takes exactly 1 argument"

/-- Python base64.b64decode(s) -/
partial def b64decode (args : List Value) : InterpM Value := do
  match args with
  | [v] => do
    let data ← toByteArray v
    match decodeBase64 data false with
    | .ok result => return .bytes result
    | .error e => throwValueError e
  | _ => throwTypeError "b64decode() takes exactly 1 argument"

/-- Python base64.urlsafe_b64encode(s) -/
partial def urlsafeB64encode (args : List Value) : InterpM Value := do
  match args with
  | [v] => do
    let data ← toByteArray v
    return .bytes (encodeBase64 data true)
  | _ => throwTypeError "urlsafe_b64encode() takes exactly 1 argument"

/-- Python base64.urlsafe_b64decode(s) -/
partial def urlsafeB64decode (args : List Value) : InterpM Value := do
  match args with
  | [v] => do
    let data ← toByteArray v
    match decodeBase64 data true with
    | .ok result => return .bytes result
    | .error e => throwValueError e
  | _ => throwTypeError "urlsafe_b64decode() takes exactly 1 argument"

/-- Python base64.b16encode(s) -/
partial def b16encode (args : List Value) : InterpM Value := do
  match args with
  | [v] => do
    let data ← toByteArray v
    return .bytes (encodeBase16 data)
  | _ => throwTypeError "b16encode() takes exactly 1 argument"

/-- Python base64.b16decode(s) -/
partial def b16decode (args : List Value) : InterpM Value := do
  match args with
  | [v] => do
    let data ← toByteArray v
    match decodeBase16 data with
    | .ok result => return .bytes result
    | .error e => throwValueError e
  | _ => throwTypeError "b16decode() takes exactly 1 argument"

end LeanPython.Stdlib.Base64
