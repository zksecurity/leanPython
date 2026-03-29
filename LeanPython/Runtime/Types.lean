import LeanPython.AST.Types
import Std.Data.HashMap

set_option autoImplicit false

namespace LeanPython.Runtime

open LeanPython.AST (Arguments Stmt Expr)
open LeanPython.Lexer (SourceSpan)

-- ============================================================
-- Heap references
-- ============================================================

/-- Opaque heap reference (index into the interpreter heap). -/
abbrev HeapRef := Nat

-- ============================================================
-- Runtime values
-- ============================================================

/-- A Python runtime value. Mutable containers (list, dict, set) use HeapRef
    indirection to model Python's reference semantics. -/
inductive Value where
  | none
  | bool      : Bool → Value
  | int       : Int → Value
  | float     : Float → Value
  | str       : String → Value
  | bytes     : ByteArray → Value
  | list      : HeapRef → Value
  | tuple     : Array Value → Value
  | dict      : HeapRef → Value
  | set       : HeapRef → Value
  | function  : HeapRef → Value
  | builtin   : String → Value
  | ellipsis

instance : Inhabited Value where
  default := .none

-- ============================================================
-- Scope
-- ============================================================

/-- A variable scope mapping names to values. -/
abbrev Scope := Std.HashMap String Value

-- ============================================================
-- Function data (stored on the heap)
-- ============================================================

/-- Captured closure: a snapshot of the scope chain at function definition time. -/
abbrev ClosureEnv := Array Scope

/-- Runtime function data stored on the heap. -/
structure FuncData where
  name       : String
  params     : Arguments
  body       : List Stmt
  defaults   : Array Value
  kwDefaults : Array (Option Value)
  closure    : ClosureEnv

-- ============================================================
-- Heap objects
-- ============================================================

/-- Objects stored on the interpreter heap. -/
inductive HeapObject where
  | listObj : Array Value → HeapObject
  | dictObj : Array (Value × Value) → HeapObject
  | setObj  : Array Value → HeapObject
  | funcObj : FuncData → HeapObject

-- ============================================================
-- Runtime errors
-- ============================================================

/-- Python-style runtime errors. -/
inductive RuntimeError where
  | nameError       : String → RuntimeError
  | typeError       : String → RuntimeError
  | valueError      : String → RuntimeError
  | indexError      : String → RuntimeError
  | keyError        : String → RuntimeError
  | zeroDivision    : String → RuntimeError
  | assertionError  : String → RuntimeError
  | attributeError  : String → RuntimeError
  | overflowError   : String → RuntimeError
  | stopIteration   : RuntimeError
  | notImplemented  : String → RuntimeError
  | runtimeError    : String → RuntimeError
  deriving Repr

instance : ToString RuntimeError where
  toString
    | .nameError s       => s!"NameError: {s}"
    | .typeError s       => s!"TypeError: {s}"
    | .valueError s      => s!"ValueError: {s}"
    | .indexError s      => s!"IndexError: {s}"
    | .keyError s        => s!"KeyError: {s}"
    | .zeroDivision s    => s!"ZeroDivisionError: {s}"
    | .assertionError s  => s!"AssertionError: {s}"
    | .attributeError s  => s!"AttributeError: {s}"
    | .overflowError s   => s!"OverflowError: {s}"
    | .stopIteration     => "StopIteration"
    | .notImplemented s  => s!"NotImplementedError: {s}"
    | .runtimeError s    => s!"RuntimeError: {s}"

-- ============================================================
-- BEq Value (needed for dict lookup, == operator, in operator)
-- ============================================================

/-- Structural equality for values. Mutable containers compare by reference
    (heap address), matching Python's default `is` semantics for objects.
    For `==` semantics, use `valueEq` in Ops.lean which does deep comparison. -/
partial def Value.beq : Value → Value → Bool
  | .none, .none => true
  | .bool a, .bool b => a == b
  | .int a, .int b => a == b
  | .float a, .float b => a == b
  | .str a, .str b => a == b
  | .bytes a, .bytes b => a == b
  | .list a, .list b => a == b
  | .tuple a, .tuple b => a.size == b.size && (List.range a.size).all fun i =>
      match a[i]?, b[i]? with
      | some va, some vb => Value.beq va vb
      | _, _ => false
  | .dict a, .dict b => a == b
  | .set a, .set b => a == b
  | .function a, .function b => a == b
  | .builtin a, .builtin b => a == b
  | .ellipsis, .ellipsis => true
  -- Cross-type: bool/int interop (Python: True == 1, False == 0)
  | .bool a, .int b => (if a then 1 else 0) == b
  | .int a, .bool b => a == (if b then 1 else 0)
  -- int/float comparison
  | .int a, .float b => Float.ofInt a == b
  | .float a, .int b => a == Float.ofInt b
  | .bool a, .float b => Float.ofInt (if a then 1 else 0) == b
  | .float a, .bool b => a == Float.ofInt (if b then 1 else 0)
  | _, _ => false

instance : BEq Value where
  beq := Value.beq

-- ============================================================
-- Value display
-- ============================================================

/-- Convert a Value to its Python `str()` representation. -/
partial def Value.toStr : Value → String
  | .none => "None"
  | .bool true => "True"
  | .bool false => "False"
  | .int n => toString n
  | .float f =>
    let s := toString f
    -- Lean's Float.toString may not match Python exactly, but good enough
    s
  | .str s => s
  | .bytes _b => "b'...'"
  | .list _ => "[...]"
  | .tuple elems =>
    if elems.size == 1 then
      s!"({Value.toStr elems[0]!},)"
    else
      "(" ++ ", ".intercalate (elems.toList.map Value.toStr) ++ ")"
  | .dict _ => "{...}"
  | .set _ => "{...}"
  | .function _ => "<function>"
  | .builtin name => s!"<built-in function {name}>"
  | .ellipsis => "Ellipsis"

/-- Convert a Value to its Python `repr()` representation. -/
partial def Value.toRepr : Value → String
  | .none => "None"
  | .bool true => "True"
  | .bool false => "False"
  | .int n => toString n
  | .float f => toString f
  | .str s => s!"'{s}'"
  | .bytes _b => "b'...'"
  | .list _ => "[...]"
  | .tuple elems =>
    if elems.size == 1 then
      s!"({Value.toRepr elems[0]!},)"
    else
      "(" ++ ", ".intercalate (elems.toList.map Value.toRepr) ++ ")"
  | .dict _ => "{...}"
  | .set _ => "{...}"
  | .function _ => "<function>"
  | .builtin name => s!"<built-in function {name}>"
  | .ellipsis => "Ellipsis"

instance : ToString Value where
  toString := Value.toStr

-- ============================================================
-- Builtin name table
-- ============================================================

/-- Names of all built-in functions recognized by the interpreter. -/
def builtinNames : List String :=
  ["print", "len", "range", "type", "int", "str", "bool", "float",
   "list", "tuple", "set", "dict", "isinstance", "repr", "abs",
   "min", "max", "sorted", "reversed", "enumerate", "zip",
   "sum", "any", "all", "hash", "id", "input", "ord", "chr",
   "hex", "oct", "bin", "round", "pow", "divmod", "map", "filter",
   "iter", "next", "hasattr", "getattr", "setattr", "callable",
   "issubclass", "super", "object"]

/-- Check if a name is a built-in function. -/
def isBuiltinName (name : String) : Bool :=
  builtinNames.contains name

end LeanPython.Runtime
