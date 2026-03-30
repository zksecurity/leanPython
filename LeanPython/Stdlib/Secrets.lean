import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Stdlib.Secrets

open LeanPython.Runtime
open LeanPython.Interpreter

/-- Generate n random bytes. -/
partial def tokenBytes (args : List Value) : InterpM Value := do
  let n := match args with
    | [.int n] => n.toNat
    | [] => 32
    | _ => 32
  let mut result := ByteArray.empty
  for _ in [:n] do
    let byte ← (IO.rand 0 255 : IO Nat)
    result := result.push (UInt8.ofNat byte)
  return .bytes result

/-- Generate a random integer in [0, n). -/
partial def randbelow (args : List Value) : InterpM Value := do
  match args with
  | [.int n] =>
    if n <= 0 then throwValueError "Upper bound must be positive"
    let val ← (IO.rand 0 (n.toNat - 1) : IO Nat)
    return .int val
  | _ => throwTypeError "secrets.randbelow() requires an integer argument"

end LeanPython.Stdlib.Secrets
