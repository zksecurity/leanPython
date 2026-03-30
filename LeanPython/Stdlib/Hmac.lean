import LeanPython.Interpreter.Types
import LeanPython.Stdlib.Hashlib

set_option autoImplicit false

namespace LeanPython.Stdlib.Hmac

open LeanPython.Runtime
open LeanPython.Interpreter
open LeanPython.Stdlib.Hashlib (sha256 bytesToHex)

-- ============================================================
-- HMAC-SHA256 (RFC 2104)
-- ============================================================

/-- Compute HMAC-SHA256(key, msg). -/
partial def hmacSha256 (key : ByteArray) (msg : ByteArray) : ByteArray := Id.run do
  let blockSize := 64
  -- If key > blockSize, hash it first
  let key' := if key.size > blockSize then sha256 key else key
  -- Pad key to blockSize with zeros
  let mut paddedKey := key'
  while paddedKey.size < blockSize do
    paddedKey := paddedKey.push 0
  -- Compute inner and outer padded keys
  let mut iKeyPad := ByteArray.empty
  let mut oKeyPad := ByteArray.empty
  for i in [:blockSize] do
    iKeyPad := iKeyPad.push ((paddedKey.get! i) ^^^ 0x36)
    oKeyPad := oKeyPad.push ((paddedKey.get! i) ^^^ 0x5C)
  -- HMAC = H(oKeyPad || H(iKeyPad || msg))
  let innerHash := sha256 (iKeyPad ++ msg)
  sha256 (oKeyPad ++ innerHash)

-- ============================================================
-- HMAC object method dispatch
-- ============================================================

/-- Dispatch a method call on an HMAC instance.
    The instance stores `_key` (bytes), `_msg` (bytes). -/
partial def callHmacMethod (iref : HeapRef) (method : String) (args : List Value)
    : InterpM Value := do
  let id_ ← heapGetInstanceData iref
  let key := match id_.attrs["_key"]? with
    | some (.bytes b) => b
    | _ => ByteArray.empty
  let msg := match id_.attrs["_msg"]? with
    | some (.bytes b) => b
    | _ => ByteArray.empty
  match method with
  | "update" =>
    match args with
    | [.bytes data] =>
      let attrs := id_.attrs.insert "_msg" (.bytes (msg ++ data))
      heapSetInstanceData iref { id_ with attrs := attrs }
      return .none
    | _ => throwTypeError "hmac.update() requires bytes"
  | "digest" =>
    return .bytes (hmacSha256 key msg)
  | "hexdigest" =>
    return .str (bytesToHex (hmacSha256 key msg))
  | "copy" =>
    let cls := id_.cls
    let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := id_.attrs })
    return .instance instRef
  | _ => throwAttributeError s!"'hmac.HMAC' object has no attribute '{method}'"

end LeanPython.Stdlib.Hmac
