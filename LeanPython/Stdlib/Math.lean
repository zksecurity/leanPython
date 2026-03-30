import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Stdlib.Math

open LeanPython.Runtime
open LeanPython.Interpreter

-- ============================================================
-- Pure math helpers wrapping Lean Float operations
-- ============================================================

/-- Convert a Float to Int, handling negative values correctly. -/
private def floatToInt (f : Float) : Int :=
  if f >= 0.0 then Int.ofNat f.toUInt64.toNat
  else -Int.ofNat (Float.toUInt64 (Float.abs f)).toNat

/-- Python math.ceil: return smallest integer >= x -/
partial def mathCeil (args : List Value) : InterpM Value := do
  match args with
  | [.float f] => return .int (floatToInt (Float.ceil f))
  | [.int n] => return .int n
  | [.bool b] => return .int (if b then 1 else 0)
  | _ => throwTypeError "math.ceil() requires a numeric argument"

/-- Python math.floor: return largest integer <= x -/
partial def mathFloor (args : List Value) : InterpM Value := do
  match args with
  | [.float f] => return .int (floatToInt (Float.floor f))
  | [.int n] => return .int n
  | [.bool b] => return .int (if b then 1 else 0)
  | _ => throwTypeError "math.floor() requires a numeric argument"

/-- Python math.sqrt: return square root of x -/
partial def mathSqrt (args : List Value) : InterpM Value := do
  match args with
  | [.float f] =>
    if f < 0.0 then throwValueError "math domain error"
    else return .float (Float.sqrt f)
  | [.int n] =>
    if n < 0 then throwValueError "math domain error"
    else return .float (Float.sqrt (Float.ofInt n))
  | [.bool b] => return .float (Float.sqrt (if b then 1.0 else 0.0))
  | _ => throwTypeError "math.sqrt() requires a numeric argument"

/-- Python math.log2: return base-2 logarithm of x -/
partial def mathLog2 (args : List Value) : InterpM Value := do
  match args with
  | [.float f] =>
    if f <= 0.0 then throwValueError "math domain error"
    else return .float (Float.log f / Float.log 2.0)
  | [.int n] =>
    if n <= 0 then throwValueError "math domain error"
    else return .float (Float.log (Float.ofInt n) / Float.log 2.0)
  | _ => throwTypeError "math.log2() requires a numeric argument"

/-- Python math.log: return natural logarithm, or log base b -/
partial def mathLog (args : List Value) : InterpM Value := do
  let toFloat : Value → InterpM Float
    | .float f => pure f
    | .int n => pure (Float.ofInt n)
    | .bool b => pure (if b then 1.0 else 0.0)
    | _ => throwTypeError "math.log() requires numeric arguments"
  match args with
  | [x] => do
    let f ← toFloat x
    if f <= 0.0 then throwValueError "math domain error"
    else return .float (Float.log f)
  | [x, base] => do
    let f ← toFloat x
    let b ← toFloat base
    if f <= 0.0 || b <= 0.0 then throwValueError "math domain error"
    else return .float (Float.log f / Float.log b)
  | _ => throwTypeError "math.log() requires 1 or 2 arguments"

/-- Python math.fabs: return absolute value as float -/
partial def mathFabs (args : List Value) : InterpM Value := do
  match args with
  | [.float f] => return .float (Float.abs f)
  | [.int n] => return .float (Float.abs (Float.ofInt n))
  | _ => throwTypeError "math.fabs() requires a numeric argument"

/-- Python math.isnan: check if float is NaN -/
partial def mathIsnan (args : List Value) : InterpM Value := do
  match args with
  | [.float f] => return .bool f.isNaN
  | [.int _] => return .bool false
  | _ => throwTypeError "math.isnan() requires a numeric argument"

/-- Python math.isinf: check if float is infinite -/
partial def mathIsinf (args : List Value) : InterpM Value := do
  match args with
  | [.float f] => return .bool f.isInf
  | [.int _] => return .bool false
  | _ => throwTypeError "math.isinf() requires a numeric argument"

end LeanPython.Stdlib.Math
