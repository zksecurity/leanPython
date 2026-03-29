import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Runtime.Ops

open LeanPython.Runtime
open LeanPython.AST (BinOp UnaryOp CmpOp BoolOp)
open LeanPython.Interpreter

-- ============================================================
-- Float comparison helper (Float has no Ord instance)
-- ============================================================

private def floatCompare (a b : Float) : Ordering :=
  if a < b then .lt
  else if a == b then .eq
  else .gt

-- ============================================================
-- Int bitwise helpers (Lean4 has bitwise on Nat, not Int directly)
-- ============================================================

private def intBitOr (a b : Int) : Int :=
  -- Simple implementation for non-negative; for negative use two's complement
  -- For now, cast through Nat for positive values
  match a, b with
  | .ofNat m, .ofNat n => .ofNat (m.lor n)
  | _, _ => a  -- TODO: handle negative int bitwise properly

private def intBitAnd (a b : Int) : Int :=
  match a, b with
  | .ofNat m, .ofNat n => .ofNat (m.land n)
  | _, _ => 0

private def intBitXor (a b : Int) : Int :=
  match a, b with
  | .ofNat m, .ofNat n => .ofNat (m.xor n)
  | _, _ => 0

private def intBitNot (a : Int) : Int :=
  -(a + 1)

-- ============================================================
-- String helpers
-- ============================================================

/-- Check if `sub` is a substring of `haystack`. -/
private def stringContains (haystack sub : String) : Bool :=
  let hLen := haystack.length
  let sLen := sub.length
  if sLen == 0 then true
  else if sLen > hLen then false
  else Id.run do
    for i in [:hLen - sLen + 1] do
      if (haystack.drop i).startsWith sub then return true
    return false

/-- Find the first occurrence of `sub` in `haystack`, returning byte offset or -1. -/
private def stringFind (haystack sub : String) : Int :=
  let hLen := haystack.length
  let sLen := sub.length
  if sLen == 0 then 0
  else if sLen > hLen then -1
  else Id.run do
    for i in [:hLen - sLen + 1] do
      if (haystack.drop i).startsWith sub then return (i : Int)
    return -1

-- ============================================================
-- Truthiness
-- ============================================================

/-- Python truthiness: determine if a value is truthy. -/
partial def isTruthy (v : Value) : InterpM Bool :=
  match v with
  | .none => return false
  | .bool b => return b
  | .int n => return (n != 0)
  | .float f => return (f != 0.0)
  | .str s => return (!s.isEmpty)
  | .bytes b => return (!b.isEmpty)
  | .list ref => do return !(← heapGetList ref).isEmpty
  | .tuple arr => return (!arr.isEmpty)
  | .dict ref => do return !(← heapGetDict ref).isEmpty
  | .set ref => do return !(← heapGetSet ref).isEmpty
  | .ellipsis => return true
  | .function _ => return true
  | .builtin _ => return true
  | .boundMethod _ _ => return true
  | .exception _ _ => return true

-- ============================================================
-- Deep equality (for == operator)
-- ============================================================

/-- Deep equality comparison matching Python's `==` semantics. -/
partial def valueEq (a b : Value) : InterpM Bool :=
  match a, b with
  | .none, .none => return true
  | .bool a, .bool b => return (a == b)
  | .int a, .int b => return (a == b)
  | .float a, .float b => return (a == b)
  | .str a, .str b => return (a == b)
  | .bytes a, .bytes b => return (a == b)
  | .ellipsis, .ellipsis => return true
  | .exception a1 a2, .exception b1 b2 => return (a1 == b1 && a2 == b2)
  -- Cross-type numeric equality
  | .bool a, .int b => return ((if a then 1 else 0) == b)
  | .int a, .bool b => return (a == (if b then 1 else 0))
  | .int a, .float b => return (Float.ofInt a == b)
  | .float a, .int b => return (a == Float.ofInt b)
  | .bool a, .float b => return (Float.ofInt (if a then 1 else 0) == b)
  | .float a, .bool b => return (a == Float.ofInt (if b then 1 else 0))
  -- Tuple: element-wise
  | .tuple a, .tuple b => do
    if a.size != b.size then return false
    for i in [:a.size] do
      if !(← valueEq a[i]! b[i]!) then return false
    return true
  -- Mutable containers: deep comparison
  | .list ra, .list rb => do
    let a ← heapGetList ra
    let b ← heapGetList rb
    if a.size != b.size then return false
    for i in [:a.size] do
      if !(← valueEq a[i]! b[i]!) then return false
    return true
  | _, _ => return false

-- ============================================================
-- Ordering (for <, <=, >, >= comparisons)
-- ============================================================

/-- Compare two numeric values, returning an Ordering. -/
private def numericCompare (a b : Value) : InterpM (Option Ordering) :=
  match a, b with
  | .int a, .int b => return some (compare a b)
  | .float a, .float b => return some (floatCompare a b)
  | .int a, .float b => return some (floatCompare (Float.ofInt a) b)
  | .float a, .int b => return some (floatCompare a (Float.ofInt b))
  | .bool a, .int b => return some (compare (if a then 1 else 0) b)
  | .int a, .bool b => return some (compare a (if b then 1 else 0))
  | .bool a, .bool b => return some (compare (if a then 1 else 0) (if b then 1 else 0 : Int))
  | .bool a, .float b => return some (floatCompare (Float.ofInt (if a then 1 else 0)) b)
  | .float a, .bool b => return some (floatCompare a (Float.ofInt (if b then 1 else 0)))
  | _, _ => return none

-- ============================================================
-- Value display (using heap access for containers)
-- ============================================================

mutual

/-- Convert a Value to its Python `str()` representation with heap access. -/
partial def valueToStr (v : Value) : InterpM String :=
  match v with
  | .none => return "None"
  | .bool true => return "True"
  | .bool false => return "False"
  | .int n => return (toString n)
  | .float f => return (toString f)
  | .str s => return s
  | .bytes _b => return "b'...'"
  | .ellipsis => return "Ellipsis"
  | .function ref => do
    let fd ← heapGetFunc ref
    return s!"<function {fd.name}>"
  | .builtin name => return s!"<built-in function {name}>"
  | .boundMethod _ method => return s!"<bound method {method}>"
  | .exception typeName msg =>
    if msg.isEmpty then return typeName
    else return s!"{typeName}({msg})"
  | .tuple elems => do
    let strs ← elems.toList.mapM valueRepr
    if elems.size == 1 then
      return s!"({strs.head!},)"
    else
      return "(" ++ ", ".intercalate strs ++ ")"
  | .list ref => do
    let arr ← heapGetList ref
    let strs ← arr.toList.mapM valueRepr
    return "[" ++ ", ".intercalate strs ++ "]"
  | .dict ref => do
    let pairs ← heapGetDict ref
    let strs ← pairs.toList.mapM fun (k, v) => do
      return s!"{← valueRepr k}: {← valueRepr v}"
    return "{" ++ ", ".intercalate strs ++ "}"
  | .set ref => do
    let elems ← heapGetSet ref
    if elems.isEmpty then return "set()"
    let strs ← elems.toList.mapM valueRepr
    return "{" ++ ", ".intercalate strs ++ "}"

/-- Convert a Value to its Python `repr()` representation with heap access. -/
partial def valueRepr (v : Value) : InterpM String :=
  match v with
  | .str s => return s!"'{s}'"
  | .bytes b => do
    let mut result := "b'"
    for byte in b.toList do
      let hi := byte.toNat / 16
      let lo := byte.toNat % 16
      let hexDigit (n : Nat) : Char := if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
      result := result ++ s!"\\x{String.ofList [hexDigit hi, hexDigit lo]}"
    return result ++ "'"
  | other => valueToStr other

end

-- ============================================================
-- Membership test (for `in` operator)
-- ============================================================

/-- Check if a value is contained in a container. -/
partial def valueContains (container elem : Value) : InterpM Bool :=
  match container with
  | .list ref => do
    let arr ← heapGetList ref
    for v in arr do
      if ← valueEq v elem then return true
    return false
  | .tuple arr => do
    for v in arr do
      if ← valueEq v elem then return true
    return false
  | .set ref => do
    let arr ← heapGetSet ref
    for v in arr do
      if ← valueEq v elem then return true
    return false
  | .dict ref => do
    let pairs ← heapGetDict ref
    for (k, _) in pairs do
      if ← valueEq k elem then return true
    return false
  | .str s => do
    match elem with
    | .str sub => return (stringContains s sub)
    | _ => throwTypeError s!"'in <string>' requires string as left operand, not {typeName elem}"
  | _ => throwTypeError s!"argument of type '{typeName container}' is not iterable"

-- ============================================================
-- Numeric helpers
-- ============================================================

/-- Convert a Value to an Int (for integer operations). -/
private def toInt : Value → Option Int
  | .int n => some n
  | .bool b => some (if b then 1 else 0)
  | _ => none

/-- Convert a Value to a Float. -/
private def toFloat : Value → Option Float
  | .float f => some f
  | .int n => some (Float.ofInt n)
  | .bool b => some (if b then 1.0 else 0.0)
  | _ => none

-- ============================================================
-- Binary operators
-- ============================================================

/-- Evaluate a binary operation. -/
partial def evalBinOp (op : BinOp) (left right : Value) : InterpM Value := do
  match op with
  | .add =>
    match left, right with
    | .int a, .int b => return .int (a + b)
    | .str a, .str b => return .str (a ++ b)
    | .list ra, .list rb => do
      let a ← heapGetList ra
      let b ← heapGetList rb
      allocList (a ++ b)
    | .tuple a, .tuple b => return .tuple (a ++ b)
    | .bytes a, .bytes b => return .bytes (a ++ b)
    | _, _ =>
      match toFloat left, toFloat right with
      | some a, some b => return .float (a + b)
      | _, _ => throwTypeError s!"unsupported operand type(s) for +: '{typeName left}' and '{typeName right}'"
  | .sub =>
    match left, right with
    | .int a, .int b => return .int (a - b)
    | .set ra, .set rb => do
      let a ← heapGetSet ra
      let b ← heapGetSet rb
      let mut result : Array Value := #[]
      for elem in a do
        let mut found := false
        for other in b do
          if ← valueEq elem other then found := true; break
        if !found then result := result.push elem
      allocSet result
    | _, _ =>
      match toFloat left, toFloat right with
      | some a, some b => return .float (a - b)
      | _, _ => throwTypeError s!"unsupported operand type(s) for -: '{typeName left}' and '{typeName right}'"
  | .mult =>
    match left, right with
    | .int a, .int b => return .int (a * b)
    | .str s, .int n | .int n, .str s =>
      if n <= 0 then return .str ""
      else return .str (String.join (List.replicate n.toNat s))
    | .list ref, .int n | .int n, .list ref => do
      let arr ← heapGetList ref
      if n <= 0 then allocList #[]
      else
        let mut result : Array Value := #[]
        for _ in [:n.toNat] do
          result := result ++ arr
        allocList result
    | _, _ =>
      match toFloat left, toFloat right with
      | some a, some b => return .float (a * b)
      | _, _ => throwTypeError s!"unsupported operand type(s) for *: '{typeName left}' and '{typeName right}'"
  | .div =>
    match toFloat left, toFloat right with
    | some a, some b =>
      if b == 0.0 then throwZeroDivision "division by zero"
      else return .float (a / b)
    | _, _ => throwTypeError s!"unsupported operand type(s) for /: '{typeName left}' and '{typeName right}'"
  | .floorDiv =>
    match left, right with
    | .int a, .int b =>
      if b == 0 then throwZeroDivision "integer division or modulo by zero"
      else return .int (Int.fdiv a b)
    | _, _ =>
      match toFloat left, toFloat right with
      | some a, some b =>
        if b == 0.0 then throwZeroDivision "integer division or modulo by zero"
        else return .float (Float.floor (a / b))
      | _, _ => throwTypeError s!"unsupported operand type(s) for //: '{typeName left}' and '{typeName right}'"
  | .mod =>
    match left, right with
    | .int a, .int b =>
      if b == 0 then throwZeroDivision "integer division or modulo by zero"
      else return .int (Int.fmod a b)
    | .str fmt, _ => do
      -- Basic % string formatting (stub)
      return .str fmt
    | _, _ =>
      match toFloat left, toFloat right with
      | some a, some b =>
        if b == 0.0 then throwZeroDivision "float modulo"
        else
          -- Python float mod: a - floor(a/b) * b
          let q := Float.floor (a / b)
          return .float (a - q * b)
      | _, _ => throwTypeError s!"unsupported operand type(s) for %: '{typeName left}' and '{typeName right}'"
  | .pow =>
    match left, right with
    | .int base, .int exp =>
      if exp < 0 then
        let bf := Float.ofInt base
        let ef := Float.ofInt exp
        return .float (bf ^ ef)
      else
        return .int (base ^ exp.toNat)
    | _, _ =>
      match toFloat left, toFloat right with
      | some a, some b => return .float (a ^ b)
      | _, _ => throwTypeError s!"unsupported operand type(s) for **: '{typeName left}' and '{typeName right}'"
  | .lShift =>
    match toInt left, toInt right with
    | some a, some b =>
      if b < 0 then throwValueError "negative shift count"
      else return .int (a <<< b.toNat)
    | _, _ => throwTypeError s!"unsupported operand type(s) for <<: '{typeName left}' and '{typeName right}'"
  | .rShift =>
    match toInt left, toInt right with
    | some a, some b =>
      if b < 0 then throwValueError "negative shift count"
      else return .int (a >>> b.toNat)
    | _, _ => throwTypeError s!"unsupported operand type(s) for >>: '{typeName left}' and '{typeName right}'"
  | .bitOr =>
    match toInt left, toInt right with
    | some a, some b => return .int (intBitOr a b)
    | _, _ =>
      match left, right with
      | .set ra, .set rb => do
        let a ← heapGetSet ra
        let b ← heapGetSet rb
        let mut result := a
        for elem in b do
          let mut found := false
          for existing in result do
            if ← valueEq existing elem then found := true; break
          if !found then result := result.push elem
        allocSet result
      | .dict ra, .dict rb => do
        let a ← heapGetDict ra
        let b ← heapGetDict rb
        let mut result := a
        for (k, v) in b do
          let mut found := false
          for i in [:result.size] do
            if ← valueEq result[i]!.1 k then
              result := result.set! i (k, v); found := true; break
          if !found then result := result.push (k, v)
        allocDict result
      | _, _ => throwTypeError s!"unsupported operand type(s) for |: '{typeName left}' and '{typeName right}'"
  | .bitAnd =>
    match toInt left, toInt right with
    | some a, some b => return .int (intBitAnd a b)
    | _, _ =>
      match left, right with
      | .set ra, .set rb => do
        let a ← heapGetSet ra
        let b ← heapGetSet rb
        let mut result : Array Value := #[]
        for elem in a do
          let mut found := false
          for other in b do
            if ← valueEq elem other then found := true; break
          if found then result := result.push elem
        allocSet result
      | _, _ => throwTypeError s!"unsupported operand type(s) for &: '{typeName left}' and '{typeName right}'"
  | .bitXor =>
    match toInt left, toInt right with
    | some a, some b => return .int (intBitXor a b)
    | _, _ =>
      match left, right with
      | .set ra, .set rb => do
        let a ← heapGetSet ra
        let b ← heapGetSet rb
        -- Elements in a but not in b
        let mut result : Array Value := #[]
        for elem in a do
          let mut found := false
          for other in b do
            if ← valueEq elem other then found := true; break
          if !found then result := result.push elem
        -- Elements in b but not in a
        for elem in b do
          let mut found := false
          for other in a do
            if ← valueEq elem other then found := true; break
          if !found then result := result.push elem
        allocSet result
      | _, _ => throwTypeError s!"unsupported operand type(s) for ^: '{typeName left}' and '{typeName right}'"
  | .matMult =>
    throwTypeError s!"unsupported operand type(s) for @: '{typeName left}' and '{typeName right}'"

-- ============================================================
-- Unary operators
-- ============================================================

/-- Evaluate a unary operation. -/
def evalUnaryOp (op : UnaryOp) (v : Value) : InterpM Value :=
  match op with
  | .uAdd =>
    match v with
    | .int n => return .int n
    | .float f => return .float f
    | .bool b => return .int (if b then 1 else 0)
    | _ => throwTypeError s!"bad operand type for unary +: '{typeName v}'"
  | .uSub =>
    match v with
    | .int n => return .int (-n)
    | .float f => return .float (-f)
    | .bool b => return .int (if b then -1 else 0)
    | _ => throwTypeError s!"bad operand type for unary -: '{typeName v}'"
  | .not_ => do
    let t ← isTruthy v
    return .bool (!t)
  | .invert =>
    match v with
    | .int n => return .int (intBitNot n)
    | .bool b => return .int (intBitNot (if b then 1 else 0))
    | _ => throwTypeError s!"bad operand type for unary ~: '{typeName v}'"

-- ============================================================
-- Comparison operators
-- ============================================================

/-- Identity comparison (Python `is`). -/
private def isIdentical : Value → Value → Bool
  | .none, .none => true
  | .bool a, .bool b => a == b
  | .int a, .int b => a == b
  | .str a, .str b => a == b
  | .list a, .list b => a == b
  | .dict a, .dict b => a == b
  | .set a, .set b => a == b
  | .function a, .function b => a == b
  | .builtin a, .builtin b => a == b
  | .ellipsis, .ellipsis => true
  | _, _ => false

/-- Evaluate a single comparison operation. -/
partial def evalCmpOp (op : CmpOp) (left right : Value) : InterpM Bool :=
  match op with
  | .eq => valueEq left right
  | .notEq => do return !(← valueEq left right)
  | .lt => do
    match ← numericCompare left right with
    | some .lt => return true
    | some _ => return false
    | none =>
      match left, right with
      | .str a, .str b => return (a < b)
      | _, _ => throwTypeError s!"'<' not supported between instances of '{typeName left}' and '{typeName right}'"
  | .ltE => do
    match ← numericCompare left right with
    | some .gt => return false
    | some _ => return true
    | none =>
      match left, right with
      | .str a, .str b => return (decide (a ≤ b))
      | _, _ => throwTypeError s!"'<=' not supported between instances of '{typeName left}' and '{typeName right}'"
  | .gt => do
    match ← numericCompare left right with
    | some .gt => return true
    | some _ => return false
    | none =>
      match left, right with
      | .str a, .str b => return (decide (a > b))
      | _, _ => throwTypeError s!"'>' not supported between instances of '{typeName left}' and '{typeName right}'"
  | .gtE => do
    match ← numericCompare left right with
    | some .lt => return false
    | some _ => return true
    | none =>
      match left, right with
      | .str a, .str b => return (decide (a ≥ b))
      | _, _ => throwTypeError s!"'>=' not supported between instances of '{typeName left}' and '{typeName right}'"
  | .is_ => return (isIdentical left right)
  | .isNot => return (!isIdentical left right)
  | .in_ => valueContains right left
  | .notIn => do return !(← valueContains right left)

-- ============================================================
-- Bool operators (short-circuit)
-- ============================================================

/-- Evaluate a boolean operation with short-circuit semantics.
    Note: Python `and`/`or` return one of the operands, not a bool. -/
partial def evalBoolOp (op : BoolOp) (vals : List Value)
    (evalNext : LeanPython.AST.Expr → InterpM Value) (exprs : List LeanPython.AST.Expr)
    : InterpM Value := do
  match op with
  | .and_ => do
    let mut result := vals.head!
    for e in exprs do
      if !(← isTruthy result) then return result
      result ← evalNext e
    return result
  | .or_ => do
    let mut result := vals.head!
    for e in exprs do
      if ← isTruthy result then return result
      result ← evalNext e
    return result

-- ============================================================
-- Iteration helper
-- ============================================================

/-- Get all values from an iterable container. -/
partial def iterValues (v : Value) : InterpM (Array Value) :=
  match v with
  | .list ref => heapGetList ref
  | .tuple arr => return arr
  | .str s => return (s.toList.map (fun c => Value.str (String.ofList [c]))).toArray
  | .dict ref => do
    let pairs ← heapGetDict ref
    return (pairs.map Prod.fst)
  | .set ref => heapGetSet ref
  | .bytes b => return (b.toList.map (fun byte => Value.int byte.toNat)).toArray
  | .boundMethod _ _ => throwTypeError s!"'{typeName v}' object is not iterable"
  | .exception _ _ => throwTypeError s!"'{typeName v}' object is not iterable"
  | _ => throwTypeError s!"'{typeName v}' object is not iterable"

end LeanPython.Runtime.Ops
