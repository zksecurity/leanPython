import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Stdlib.Time

open LeanPython.Runtime
open LeanPython.Interpreter

-- ============================================================
-- time module functions
-- ============================================================

/-- Python time.time(): return current time as float seconds. -/
partial def timeTime (_args : List Value) : InterpM Value := do
  let ms ← (IO.monoMsNow : BaseIO Nat)
  return .float (Nat.toFloat ms / 1000.0)

/-- Python time.monotonic(): return monotonic clock as float seconds. -/
partial def timeMonotonic (_args : List Value) : InterpM Value := do
  let ms ← (IO.monoMsNow : BaseIO Nat)
  return .float (Nat.toFloat ms / 1000.0)

/-- Python time.perf_counter(): return performance counter as float seconds. -/
partial def timePerfCounter (_args : List Value) : InterpM Value := do
  let ms ← (IO.monoMsNow : BaseIO Nat)
  return .float (Nat.toFloat ms / 1000.0)

/-- Python time.sleep(seconds): sleep for given duration. -/
partial def timeSleep (args : List Value) : InterpM Value := do
  let ms : UInt32 := match args with
    | [.float f] => (f * 1000.0).toUInt32
    | [.int n] => (n * 1000).toNat.toUInt32
    | _ => 0
  IO.sleep ms
  return .none

end LeanPython.Stdlib.Time
