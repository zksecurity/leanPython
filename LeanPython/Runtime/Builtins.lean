import LeanPython.Runtime.Ops
import LeanPython.Stdlib.Math
import LeanPython.Stdlib.Struct
import LeanPython.Stdlib.IO
import LeanPython.Stdlib.Bisect
import LeanPython.Stdlib.Base64
import LeanPython.Stdlib.Json
import LeanPython.Stdlib.Hashlib
import LeanPython.Stdlib.Hmac
import LeanPython.Stdlib.Secrets
import LeanPython.Stdlib.Sys
import LeanPython.Stdlib.Os
import LeanPython.Stdlib.Time
import LeanPython.Stdlib.Datetime
import LeanPython.Stdlib.Pathlib
import LeanPython.Stdlib.Logging
import LeanPython.Stdlib.Pydantic

set_option autoImplicit false

namespace LeanPython.Runtime.Builtins

open LeanPython.Runtime
open LeanPython.Runtime.Ops
open LeanPython.Interpreter
open LeanPython.Stdlib.Math
open LeanPython.Stdlib.Struct
open LeanPython.Stdlib.Bisect
open LeanPython.Stdlib.Base64
open LeanPython.Stdlib.Json
open LeanPython.Stdlib.Hashlib
open LeanPython.Stdlib.Hmac
open LeanPython.Stdlib.Secrets
open LeanPython.Stdlib.Sys
open LeanPython.Stdlib.Os
open LeanPython.Stdlib.Time
open LeanPython.Stdlib.Logging

-- ============================================================
-- Individual builtin implementations
-- ============================================================

/-- `print(*args, sep=' ', end='\n')` -/
partial def builtinPrint (args : List Value) (kwargs : List (String × Value))
    : InterpM Value := do
  let sep := match kwargs.find? (fun p => p.1 == "sep") with
    | some (_, .str s) => s
    | _ => " "
  let end_ := match kwargs.find? (fun p => p.1 == "end") with
    | some (_, .str s) => s
    | _ => "\n"
  let strs ← args.mapM valueToStr
  let line := String.intercalate sep strs ++ end_
  emitOutput line
  return .none

/-- `len(obj)` -/
partial def builtinLen (args : List Value) : InterpM Value := do
  match args with
  | [v] =>
    match v with
    | .str s => return .int s.length
    | .bytes b => return .int b.size
    | .list ref => do return .int (← heapGetList ref).size
    | .tuple arr => return .int arr.size
    | .dict ref => do return .int (← heapGetDict ref).size
    | .set ref => do return .int (← heapGetSet ref).size
    | _ => throwTypeError s!"object of type '{typeName v}' has no len()"
  | _ => throwTypeError "len() takes exactly one argument"

/-- `range(stop)` or `range(start, stop[, step])` - materializes to list. -/
partial def builtinRange (args : List Value) : InterpM Value := do
  let (start_, stop_, step_) ← match args with
    | [.int n] => pure (0, n, (1 : Int))
    | [.int s, .int e] => pure (s, e, (1 : Int))
    | [.int s, .int e, .int st] => pure (s, e, st)
    | [.bool b] => pure (0, (if b then 1 else 0 : Int), (1 : Int))
    | _ => throwTypeError "range() requires int arguments"
  if step_ == 0 then
    throwValueError "range() arg 3 must not be zero"
  let mut elems : Array Value := #[]
  let mut i := start_
  if step_ > 0 then
    while i < stop_ do
      elems := elems.push (.int i)
      i := i + step_
  else
    while i > stop_ do
      elems := elems.push (.int i)
      i := i + step_
  allocList elems

/-- `type(obj)` - returns the type of an object. -/
partial def builtinType (args : List Value) : InterpM Value := do
  match args with
  | [v] =>
    match v with
    | .instance ref => do
      let id_ ← heapGetInstanceData ref
      return id_.cls
    | .exception typeName _ => return .builtin typeName
    | _ => return .str s!"<class '{typeName v}'>"
  | _ => throwTypeError "type() takes 1 or 3 arguments"

/-- `int(x)` - convert to int. -/
def builtinInt (args : List Value) : InterpM Value := do
  match args with
  | [] => return .int 0
  | [.int n] => return .int n
  | [.bool b] => return .int (if b then 1 else 0)
  | [.float f] => return .int (Float.toUInt64 f).toNat  -- truncates toward zero
  | [.str s] =>
    match s.toList.dropWhile Char.isWhitespace |>.reverse |>.dropWhile Char.isWhitespace |>.reverse |> String.ofList |>.toInt? with
    | some n => return .int n
    | none => throwValueError s!"invalid literal for int() with base 10: '{s}'"
  | [.instance iref] => do
    let id_ ← heapGetInstanceData iref
    match id_.wrappedValue with
    | some (.int n) => return .int n
    | _ => throwTypeError "int() argument must be a string or a number"
  | _ => throwTypeError "int() argument must be a string or a number"

/-- `float(x)` - convert to float. -/
def builtinFloat (args : List Value) : InterpM Value := do
  match args with
  | [] => return .float 0.0
  | [.float f] => return .float f
  | [.int n] => return .float (Float.ofInt n)
  | [.bool b] => return .float (if b then 1.0 else 0.0)
  | [.str s] =>
    -- Try to parse float from string
    -- Lean doesn't have a built-in String.toFloat?, so basic parsing
    match s.toList.dropWhile Char.isWhitespace |>.reverse |>.dropWhile Char.isWhitespace |>.reverse |> String.ofList |>.toInt? with
    | some n => return .float (Float.ofInt n)
    | none => throwValueError s!"could not convert string to float: '{s}'"
  | _ => throwTypeError "float() argument must be a string or a number"

/-- `str(x)` - convert to string. -/
partial def builtinStr (args : List Value) : InterpM Value := do
  match args with
  | [] => return .str ""
  | [v] => return .str (← valueToStr v)
  | _ => throwTypeError "str() takes at most 1 argument"

/-- `bool(x)` - convert to bool. -/
partial def builtinBool (args : List Value) : InterpM Value := do
  match args with
  | [] => return .bool false
  | [v] => return .bool (← isTruthy v)
  | _ => throwTypeError "bool() takes at most 1 argument"

/-- `repr(x)` - return repr string. -/
partial def builtinRepr (args : List Value) : InterpM Value := do
  match args with
  | [v] => return .str (← valueRepr v)
  | _ => throwTypeError "repr() takes exactly one argument"

/-- `abs(x)` - absolute value. -/
def builtinAbs (args : List Value) : InterpM Value := do
  match args with
  | [.int n] => return .int n.natAbs
  | [.float f] => return .float f.abs
  | [.bool b] => return .int (if b then 1 else 0)
  | _ => throwTypeError "abs() argument must be a number"

/-- `min(*args)` or `min(iterable)` -/
partial def builtinMin (args : List Value) : InterpM Value := do
  let items : List Value ← match args with
    | [v] => do let arr ← iterValues v; pure arr.toList
    | xs => pure xs
  match items with
  | [] => throwValueError "min() arg is an empty sequence"
  | first :: rest => do
    let mut best := first
    for v in rest do
      if ← evalCmpOp .lt v best then
        best := v
    return best

/-- `max(*args)` or `max(iterable)` -/
partial def builtinMax (args : List Value) : InterpM Value := do
  let items : List Value ← match args with
    | [v] => do let arr ← iterValues v; pure arr.toList
    | xs => pure xs
  match items with
  | [] => throwValueError "max() arg is an empty sequence"
  | first :: rest => do
    let mut best := first
    for v in rest do
      if ← evalCmpOp .gt v best then
        best := v
    return best

/-- `sum(iterable, start=0)` -/
partial def builtinSum (args : List Value) : InterpM Value := do
  match args with
  | [iter] => do
    let items ← iterValues iter
    let mut acc : Value := .int 0
    for v in items do
      acc ← evalBinOp .add acc v
    return acc
  | [iter, start] => do
    let items ← iterValues iter
    let mut acc := start
    for v in items do
      acc ← evalBinOp .add acc v
    return acc
  | _ => throwTypeError "sum() takes at most 2 arguments"

/-- `any(iterable)` -/
partial def builtinAny (args : List Value) : InterpM Value := do
  match args with
  | [iter] => do
    let items ← iterValues iter
    for v in items do
      if ← isTruthy v then return .bool true
    return .bool false
  | _ => throwTypeError "any() takes exactly one argument"

/-- `all(iterable)` -/
partial def builtinAll (args : List Value) : InterpM Value := do
  match args with
  | [iter] => do
    let items ← iterValues iter
    for v in items do
      if !(← isTruthy v) then return .bool false
    return .bool true
  | _ => throwTypeError "all() takes exactly one argument"

/-- `sorted(iterable)` - basic insertion sort for now. -/
partial def builtinSorted (args : List Value) : InterpM Value := do
  match args with
  | [iter] => do
    let items := (← iterValues iter).toList
    -- Simple insertion sort
    let sorted ← insertionSort items
    allocList sorted.toArray
  | _ => throwTypeError "sorted() takes 1 argument"
where
  insertionSort (xs : List Value) : InterpM (List Value) := do
    let mut result : List Value := []
    for x in xs do
      result ← insert x result
    return result
  insert (x : Value) (sorted : List Value) : InterpM (List Value) := do
    match sorted with
    | [] => return [x]
    | y :: ys =>
      if ← evalCmpOp .ltE x y then return x :: y :: ys
      else return y :: (← insert x ys)

/-- `reversed(seq)` - returns reversed list. -/
partial def builtinReversed (args : List Value) : InterpM Value := do
  match args with
  | [v] => do
    let items ← iterValues v
    allocList items.reverse
  | _ => throwTypeError "reversed() takes exactly one argument"

/-- `enumerate(iterable, start=0)` - returns list of (index, value) tuples. -/
partial def builtinEnumerate (args : List Value) : InterpM Value := do
  let (iter, start) ← match args with
    | [iter] => pure (iter, (0 : Int))
    | [iter, .int s] => pure (iter, s)
    | _ => throwTypeError "enumerate() takes 1 or 2 arguments"
  let items ← iterValues iter
  let mut result : Array Value := #[]
  let mut idx := start
  for v in items do
    result := result.push (.tuple #[.int idx, v])
    idx := idx + 1
  allocList result

/-- `zip(*iterables)` - returns list of tuples. -/
partial def builtinZip (args : List Value) : InterpM Value := do
  let iterables ← args.mapM iterValues
  if iterables.isEmpty then
    allocList #[]
  else
    let minLen := iterables.foldl (fun acc arr => min acc arr.size) iterables.head!.size
    let mut result : Array Value := #[]
    for i in [:minLen] do
      let tuple := iterables.map fun arr => arr[i]!
      result := result.push (.tuple tuple.toArray)
    allocList result

/-- `list(iterable?)` - convert to list. -/
partial def builtinList (args : List Value) : InterpM Value := do
  match args with
  | [] => allocList #[]
  | [v] => do
    let items ← iterValues v
    allocList items
  | _ => throwTypeError "list() takes at most 1 argument"

/-- `tuple(iterable?)` - convert to tuple. -/
partial def builtinTuple (args : List Value) : InterpM Value := do
  match args with
  | [] => return .tuple #[]
  | [v] => do
    let items ← iterValues v
    return .tuple items
  | _ => throwTypeError "tuple() takes at most 1 argument"

/-- Check if an instance's class (or any class in its MRO) matches a target class. -/
partial def isInstanceOfClass (instRef : HeapRef) (targetCls : Value) : InterpM Bool := do
  let id_ ← heapGetInstanceData instRef
  -- Walk the MRO of the instance's class
  match id_.cls with
  | .classObj cref => do
    let cd ← heapGetClassData cref
    -- Check each class in the MRO
    for mroEntry in cd.mro do
      if Value.beq mroEntry targetCls then return true
    return false
  | _ => return false

/-- `isinstance(obj, classinfo)` - type check for both builtins and custom classes. -/
partial def builtinIsinstance (args : List Value) : InterpM Value := do
  match args with
  | [obj, .str className] =>
    return .bool (typeName obj == className)
  | [obj, .builtin name] =>
    -- isinstance(x, int) where int is the builtin
    match name with
    | "object" => return .bool true  -- everything is an instance of object
    | _ =>
      -- First check if it's a direct built-in value match
      let tn := typeName obj
      let isMatch := match name with
        | "int" => tn == "int" || tn == "bool"
        | "float" => tn == "float"
        | "str" => tn == "str"
        | "bool" => tn == "bool"
        | "list" => tn == "list"
        | "tuple" => tn == "tuple"
        | "dict" => tn == "dict"
        | "set" => tn == "set"
        | "bytes" => tn == "bytes"
        | _ => false
      if isMatch then return .bool true
      -- For instances, check if any class in MRO is the synthetic built-in type
      match obj with
      | .instance iref => do
        let id_ ← heapGetInstanceData iref
        match id_.cls with
        | .classObj cref => do
          let cd ← heapGetClassData cref
          let mut found := false
          for mroEntry in cd.mro do
            match mroEntry with
            | .classObj mref => do
              let mcd ← heapGetClassData mref
              if mcd.name == name then found := true
            | _ => pure ()
          return .bool found
        | _ => return .bool false
      | _ => return .bool false
  | [.instance instRef, .classObj _] =>
    return .bool (← isInstanceOfClass instRef args[1]!)
  | [.instance instRef, .tuple classes] => do
    -- isinstance(obj, (ClassA, ClassB, ...))
    for cls in classes do
      match cls with
      | .classObj _ =>
        if ← isInstanceOfClass instRef cls then return .bool true
      | .builtin bname => do
        if bname == "object" then return .bool true
        -- Check MRO for synthetic built-in type
        let id_ ← heapGetInstanceData instRef
        match id_.cls with
        | .classObj cref => do
          let cd ← heapGetClassData cref
          for mroEntry in cd.mro do
            match mroEntry with
            | .classObj mref => do
              let mcd ← heapGetClassData mref
              if mcd.name == bname then return .bool true
            | _ => pure ()
        | _ => pure ()
      | _ => pure ()
    return .bool false
  | [_, .classObj _] => return .bool false  -- non-instance is not an instance of a custom class
  | _ => throwTypeError "isinstance() takes 2 arguments"

/-- Hash a single value for tuple/object hashing. -/
private partial def hashValue : Value → InterpM Int
  | .int n => pure n
  | .bool b => pure (if b then 1 else 0)
  | .str s => pure (hash s).toNat
  | .none => pure 0x345678
  | .float f => pure (hash (toString f)).toNat
  | .tuple items => do
    -- Combine element hashes using simple mixing
    let mut h : Int := 0x345678
    for item in items do
      let ih ← hashValue item
      h := h * 1000003 + ih
    pure h
  | .classObj ref => pure ref
  | .instance iref => do
    let id_ ← heapGetInstanceData iref
    match id_.wrappedValue with
    | some v => hashValue v
    | none => pure iref  -- use heap ref as fallback
  | _ => pure 0

/-- `hash(obj)` - basic hash. -/
partial def builtinHash (args : List Value) : InterpM Value := do
  match args with
  | [v] => return .int (← hashValue v)
  | _ => throwTypeError "hash() takes exactly 1 argument"

/-- `id(obj)` - identity (stub). -/
def builtinId (args : List Value) : InterpM Value := do
  match args with
  | [.list ref] => return .int ref
  | [.dict ref] => return .int ref
  | [.set ref] => return .int ref
  | [.function ref] => return .int ref
  | [_] => return .int 0  -- non-heap objects get dummy id
  | _ => throwTypeError "id() takes exactly one argument"

/-- `ord(c)` - Unicode code point. -/
def builtinOrd (args : List Value) : InterpM Value := do
  match args with
  | [.str s] =>
    if s.length == 1 then return .int s.front.toNat
    else throwTypeError "ord() expected a character, but string of length {s.length} found"
  | _ => throwTypeError "ord() expected string of length 1"

/-- `chr(i)` - Character from code point. -/
def builtinChr (args : List Value) : InterpM Value := do
  match args with
  | [.int n] =>
    if n >= 0 && n <= 0x10FFFF then return .str (String.ofList [Char.ofNat n.toNat])
    else throwValueError "chr() arg not in range(0x110000)"
  | _ => throwTypeError "chr() takes exactly one argument"

/-- `pow(base, exp[, mod])` -/
def builtinPow (args : List Value) : InterpM Value := do
  match args with
  | [base, exp] => evalBinOp .pow base exp
  | [.int base, .int exp, .int m] =>
    if m == 0 then throwValueError "pow() 3rd argument cannot be 0"
    else if exp < 0 then throwValueError "pow() 2nd argument cannot be negative when 3rd argument specified"
    else
      -- modular exponentiation
      let mut result : Int := 1
      let mut b := Int.fmod base m
      let mut e := exp
      while e > 0 do
        if Int.fmod e 2 == 1 then result := Int.fmod (result * b) m
        e := e / 2
        b := Int.fmod (b * b) m
      return .int result
  | _ => throwTypeError "pow() takes 2 or 3 arguments"

/-- `divmod(a, b)` -/
def builtinDivmod (args : List Value) : InterpM Value := do
  match args with
  | [.int a, .int b] =>
    if b == 0 then throwZeroDivision "integer division or modulo by zero"
    else return .tuple #[.int (Int.fdiv a b), .int (Int.fmod a b)]
  | _ => throwTypeError "divmod() requires two integer arguments"

/-- `callable(obj)` -/
def builtinCallable (args : List Value) : InterpM Value := do
  match args with
  | [v] => return .bool (match v with
    | .function _ => true
    | .builtin _ => true
    | .boundMethod _ _ => true
    | .classObj _ => true
    | _ => false)
  | _ => throwTypeError "callable() takes exactly one argument"

-- ============================================================
-- copy module helpers
-- ============================================================

/-- Shallow copy of a Python value. -/
partial def shallowCopy (v : Value) : InterpM Value := do
  match v with
  | .list ref => do
    let items ← heapGetList ref
    allocList items
  | .dict ref => do
    let pairs ← heapGetDict ref
    allocDict pairs
  | .set ref => do
    let items ← heapGetSet ref
    allocSet items
  | .instance ref => do
    let id_ ← heapGetInstanceData ref
    let newRef ← heapAlloc (.instanceObjData { cls := id_.cls, attrs := id_.attrs })
    return .instance newRef
  -- Immutable types: return as-is
  | v => return v

/-- Deep copy of a Python value (no cycle detection). -/
partial def deepCopy (v : Value) : InterpM Value := do
  match v with
  | .list ref => do
    let items ← heapGetList ref
    let mut copied := #[]
    for item in items do
      copied := copied.push (← deepCopy item)
    allocList copied
  | .dict ref => do
    let pairs ← heapGetDict ref
    let mut copied := #[]
    for (k, val) in pairs do
      copied := copied.push (← deepCopy k, ← deepCopy val)
    allocDict copied
  | .set ref => do
    let items ← heapGetSet ref
    let mut copied := #[]
    for item in items do
      copied := copied.push (← deepCopy item)
    allocSet copied
  | .instance ref => do
    let id_ ← heapGetInstanceData ref
    let mut attrs : Std.HashMap String Value := {}
    for (k, val) in id_.attrs.toList do
      attrs := attrs.insert k (← deepCopy val)
    let newRef ← heapAlloc (.instanceObjData { cls := id_.cls, attrs := attrs })
    return .instance newRef
  | .tuple items => do
    let mut copied := #[]
    for item in items do
      copied := copied.push (← deepCopy item)
    return .tuple copied
  -- Immutable types: return as-is
  | v => return v

-- ============================================================
-- Main dispatch
-- ============================================================

/-- Dispatch a builtin function call by name. -/
partial def callBuiltin (name : String) (args : List Value)
    (kwargs : List (String × Value)) : InterpM Value :=
  match name with
  | "print"      => builtinPrint args kwargs
  | "len"        => builtinLen args
  | "range"      => builtinRange args
  | "type"       => builtinType args
  | "int"        => builtinInt args
  | "float"      => builtinFloat args
  | "str"        => builtinStr args
  | "bool"       => builtinBool args
  | "repr"       => builtinRepr args
  | "abs"        => builtinAbs args
  | "min"        => builtinMin args
  | "max"        => builtinMax args
  | "sum"        => builtinSum args
  | "any"        => builtinAny args
  | "all"        => builtinAll args
  | "sorted"     => builtinSorted args
  | "reversed"   => builtinReversed args
  | "enumerate"  => builtinEnumerate args
  | "zip"        => builtinZip args
  | "list"       => builtinList args
  | "tuple"      => builtinTuple args
  | "isinstance" => builtinIsinstance args
  | "issubclass" => do
    match args with
    | [.classObj aRef, .classObj _] => do
      let cd ← heapGetClassData aRef
      let mut found := false
      for mroEntry in cd.mro do
        if Value.beq mroEntry args[1]! then found := true; break
      return .bool found
    | [.classObj _, .builtin "object"] => return .bool true
    | [_, _] => throwTypeError "issubclass() arg 1 must be a class"
    | _ => throwTypeError "issubclass() takes 2 arguments"
  | "object" => do
    -- object() creates a base object — for now just return a plain instance
    return .none
  | "super" => do
    match args with
    | [] => do
      -- Implicit super(): look up __class__ and first param from current scope
      let cls ← lookupVariable "__class__"
      -- Try "self" first, then "cls" (for __new__/classmethods)
      let inst ← try lookupVariable "self"
        catch _ => try lookupVariable "cls"
          catch _ => throwTypeError "super(): __class__ cell not found"
      return .superObj cls inst
    | [cls, inst] => return .superObj cls inst
    | _ => throwTypeError "super() takes 0 or 2 arguments"
  | "hash"       => builtinHash args
  | "id"         => builtinId args
  | "ord"        => builtinOrd args
  | "chr"        => builtinChr args
  | "pow"        => builtinPow args
  | "divmod"     => builtinDivmod args
  | "callable"   => builtinCallable args
  | "input"      => do
    match args with
    | [] => do
      let line ← (IO.getStdin >>= IO.FS.Stream.getLine : IO String)
      return .str (line.toList.reverse.dropWhile Char.isWhitespace |>.reverse |> String.ofList)
    | [prompt] => do
      emitOutput (← valueToStr prompt)
      let line ← (IO.getStdin >>= IO.FS.Stream.getLine : IO String)
      return .str (line.toList.reverse.dropWhile Char.isWhitespace |>.reverse |> String.ofList)
    | _ => throwTypeError "input() takes at most 1 argument"
  | "hex" => do
    match args with
    | [.int n] =>
      let digits := Nat.toDigits 16 n.natAbs
      let s := String.ofList digits
      if n >= 0 then return .str s!"0x{s}"
      else return .str s!"-0x{s}"
    | _ => throwTypeError "hex() takes exactly one argument"
  | "oct" => do
    match args with
    | [.int n] =>
      let digits := Nat.toDigits 8 n.natAbs
      let s := String.ofList digits
      if n >= 0 then return .str s!"0o{s}"
      else return .str s!"-0o{s}"
    | _ => throwTypeError "oct() takes exactly one argument"
  | "bin" => do
    match args with
    | [.int n] =>
      let digits := Nat.toDigits 2 n.natAbs
      let s := String.ofList digits
      if n >= 0 then return .str s!"0b{s}"
      else return .str s!"-0b{s}"
    | _ => throwTypeError "bin() takes exactly one argument"
  | "round" => do
    match args with
    | [.int n] => return .int n
    | [.float f] => return .int (Float.round f).toUInt64.toNat
    | _ => throwTypeError "round() takes 1 argument"
  | "bytes" => do
    match args with
    | [] => return .bytes ByteArray.empty
    | [.int n] =>
      if n < 0 then throwValueError "negative count"
      else
        let mut ba := ByteArray.empty
        for _ in [:n.toNat] do ba := ba.push 0
        return .bytes ba
    | [.bytes b] => return .bytes b
    | [v] => do
      let items ← iterValues v
      let mut result := ByteArray.empty
      for item in items do
        match item with
        | .int n =>
          if n < 0 || n > 255 then throwValueError "bytes must be in range(0, 256)"
          else result := result.push n.toNat.toUInt8
        | _ => throwTypeError "cannot convert to bytes"
      return .bytes result
    | _ => throwTypeError "bytes() takes at most 1 argument"
  | "iter" => do
    match args with
    | [.generator ref] => return .generator ref
    | [v] => do
      let items ← iterValues v
      allocGenerator items
    | _ => throwTypeError "iter() takes exactly one argument"
  | "next" => do
    match args with
    | [.generator ref] => do
      let (buf, idx) ← heapGetGenerator ref
      if idx >= buf.size then throwRuntimeError .stopIteration
      heapSetGeneratorIdx ref (idx + 1)
      return buf[idx]!
    | [.generator ref, default_] => do
      let (buf, idx) ← heapGetGenerator ref
      if idx >= buf.size then return default_
      heapSetGeneratorIdx ref (idx + 1)
      return buf[idx]!
    | _ => throwTypeError "next() requires a generator/iterator argument"
  -- Exception constructors
  | "ValueError" | "TypeError" | "KeyError" | "IndexError"
  | "RuntimeError" | "ZeroDivisionError" | "AssertionError"
  | "AttributeError" | "OverflowError" | "StopIteration"
  | "NotImplementedError" | "Exception" | "BaseException"
  | "NameError" | "OSError" | "IOError" | "FileNotFoundError"
  | "ImportError" | "ModuleNotFoundError" => do
    let msg ← match args with
      | [] => pure ""
      | [v] => valueToStr v
      | _ => do
        let strs ← args.mapM valueToStr
        pure (", ".intercalate strs)
    return .exception name msg
  -- ============================================================
  -- math module functions
  -- ============================================================
  | "math.ceil"  => mathCeil args
  | "math.floor" => mathFloor args
  | "math.sqrt"  => mathSqrt args
  | "math.log"   => mathLog args
  | "math.log2"  => mathLog2 args
  | "math.fabs"  => mathFabs args
  | "math.isnan" => mathIsnan args
  | "math.isinf" => mathIsinf args
  -- ============================================================
  -- copy module functions
  -- ============================================================
  | "copy.copy" => do
    match args with
    | [v] => shallowCopy v
    | _ => throwTypeError "copy.copy() takes exactly 1 argument"
  | "copy.deepcopy" => do
    match args with
    | [v] => deepCopy v
    | _ => throwTypeError "copy.deepcopy() takes exactly 1 argument"
  -- ============================================================
  -- operator module functions
  -- ============================================================
  | "operator.add" => do
    match args with
    | [a, b] => evalBinOp .add a b
    | _ => throwTypeError "operator.add() takes exactly 2 arguments"
  | "operator.sub" => do
    match args with
    | [a, b] => evalBinOp .sub a b
    | _ => throwTypeError "operator.sub() takes exactly 2 arguments"
  | "operator.mul" => do
    match args with
    | [a, b] => evalBinOp .mult a b
    | _ => throwTypeError "operator.mul() takes exactly 2 arguments"
  | "operator.eq" => do
    match args with
    | [a, b] => .bool <$> valueEq a b
    | _ => throwTypeError "operator.eq() takes exactly 2 arguments"
  | "operator.ne" => do
    match args with
    | [a, b] => .bool <$> (not <$> valueEq a b)
    | _ => throwTypeError "operator.ne() takes exactly 2 arguments"
  | "operator.lt" => do
    match args with
    | [a, b] => .bool <$> evalCmpOp .lt a b
    | _ => throwTypeError "operator.lt() takes exactly 2 arguments"
  | "operator.le" => do
    match args with
    | [a, b] => .bool <$> evalCmpOp .ltE a b
    | _ => throwTypeError "operator.le() takes exactly 2 arguments"
  | "operator.gt" => do
    match args with
    | [a, b] => .bool <$> evalCmpOp .gt a b
    | _ => throwTypeError "operator.gt() takes exactly 2 arguments"
  | "operator.ge" => do
    match args with
    | [a, b] => .bool <$> evalCmpOp .gtE a b
    | _ => throwTypeError "operator.ge() takes exactly 2 arguments"
  | "operator.neg" => do
    match args with
    | [.int n] => return .int (-n)
    | [.float f] => return .float (-f)
    | _ => throwTypeError "operator.neg() takes exactly 1 numeric argument"
  | "operator.not_" => do
    match args with
    | [v] => return .bool (!(← isTruthy v))
    | _ => throwTypeError "operator.not_() takes exactly 1 argument"
  -- ============================================================
  -- itertools module functions (pure ones)
  -- ============================================================
  | "itertools.chain" => do
    let mut result : Array Value := #[]
    for arg in args do
      let items ← iterValues arg
      result := result ++ items
    allocGenerator result
  | "itertools.count" => do
    -- itertools.count(start=0, step=1) — generate a bounded sequence
    let start := match args.head? with
      | some (.int n) => n | _ => 0
    let step := match args.drop 1 |>.head? with
      | some (.int n) => n | _ => 1
    -- Materialize a bounded range (1000 elements max for safety)
    let mut result : Array Value := #[]
    let mut cur := start
    for _ in [:1000] do
      result := result.push (.int cur)
      cur := cur + step
    allocGenerator result
  -- ============================================================
  -- functools stubs (lru_cache, wraps as identity decorators)
  -- ============================================================
  | "functools.lru_cache" => do
    -- lru_cache can be called as @lru_cache or @lru_cache(maxsize=128)
    match args with
    | [f@(.function _)] => return f
    | [f@(.builtin _)] => return f
    | [.int _] => return .builtin "functools.lru_cache"
    | [.none] => return .builtin "functools.lru_cache"
    | [] => return .builtin "functools.lru_cache"
    | _ => match args.head? with
      | some f => return f
      | none => return .builtin "functools.lru_cache"
  | "functools.wraps" => do
    -- wraps(wrapped) returns a decorator that is identity
    match args with
    | [_] => return .builtin "functools.wraps_inner"
    | _ => throwTypeError "functools.wraps() takes exactly 1 argument"
  | "functools.wraps_inner" => do
    match args with
    | [f] => return f
    | _ => throwTypeError "functools.wraps() decorator takes exactly 1 argument"
  | "functools.partial" => do
    -- Stub: not yet implemented
    throwNotImplemented "functools.partial is not yet implemented"
  | "functools.cached_property" => do
    -- Identity decorator for now
    match args with
    | [f] => return f
    | _ => throwTypeError "functools.cached_property() takes exactly 1 argument"
  | "functools.total_ordering" => do
    -- Identity decorator for now
    match args with
    | [cls] => return cls
    | _ => throwTypeError "functools.total_ordering() takes exactly 1 argument"
  | "functools.singledispatch" => do
    -- Stub: just return the function
    match args with
    | [f] => return f
    | _ => throwTypeError "functools.singledispatch() takes exactly 1 argument"
  -- ============================================================
  -- abc module functions
  -- ============================================================
  | "abc.abstractmethod" => do
    match args with
    | [f] => return f
    | _ => throwTypeError "abstractmethod() takes exactly 1 argument"
  | "typing.override" => do
    match args with
    | [f] => return f
    | _ => throwTypeError "override() takes exactly 1 argument"
  -- ============================================================
  -- dataclasses module functions
  -- ============================================================
  -- ============================================================
  -- enum module functions
  -- ============================================================
  -- ============================================================
  -- typing module functions
  -- ============================================================
  | "typing.get_type_hints" => do
    match args with
    | [.instance ref] => do
      let id_ ← heapGetInstanceData ref
      match id_.attrs["__annotations__"]? with
      | some v => return v
      | none => allocDict #[]
    | [.classObj ref] => do
      let cd ← heapGetClassData ref
      match cd.ns["__annotations__"]? with
      | some v => return v
      | none => allocDict #[]
    | [_] => allocDict #[]
    | _ => throwTypeError "get_type_hints() takes exactly 1 argument"
  | "enum.auto" => do
    -- Returns a sentinel that gets replaced during enum class creation
    return .builtin "enum.auto_value"
  | "enum.auto_value" => do
    -- Should not be called directly
    throwTypeError "auto() value should not be called directly"
  | "dataclasses.field" => do
    -- field(default=..., default_factory=...) returns a sentinel tuple
    let default_ := kwargs.find? (fun p => p.1 == "default") |>.map (·.2)
    let factory := kwargs.find? (fun p => p.1 == "default_factory") |>.map (·.2)
    return .tuple #[.str "__dataclass_field__",
                    default_.getD .none,
                    factory.getD .none]
  | "dataclasses.fields" => do
    -- Stub: returns empty tuple for now
    match args with
    | [_] => return .tuple #[]
    | _ => throwTypeError "dataclasses.fields() takes exactly 1 argument"
  -- ============================================================
  -- struct module functions
  -- ============================================================
  | "struct.pack"     => structPack args
  | "struct.unpack"   => structUnpack args
  | "struct.calcsize" => structCalcsize args
  -- ============================================================
  -- bisect module functions
  -- ============================================================
  | "bisect.bisect_left"  => bisectLeft args kwargs
  | "bisect.bisect_right" => bisectRight args kwargs
  | "bisect.bisect"       => bisectRight args kwargs  -- bisect is alias for bisect_right
  | "bisect.insort"       => bisectInsort args kwargs
  | "bisect.insort_right" => bisectInsort args kwargs
  | "bisect.insort_left"  => bisectInsort args kwargs  -- simplified
  -- ============================================================
  -- base64 module functions
  -- ============================================================
  | "base64.b64encode"           => b64encode args
  | "base64.b64decode"           => b64decode args
  | "base64.urlsafe_b64encode"   => urlsafeB64encode args
  | "base64.urlsafe_b64decode"   => urlsafeB64decode args
  | "base64.b16encode"           => b16encode args
  | "base64.b16decode"           => b16decode args
  -- ============================================================
  -- json module functions
  -- ============================================================
  | "json.dumps" => jsonDumps args kwargs
  | "json.loads" => jsonLoads args
  -- ============================================================
  -- sys module functions
  -- ============================================================
  | "sys.exit" => sysExit args
  -- ============================================================
  -- os module functions
  -- ============================================================
  | "os.getcwd" => osGetcwd args
  | "os.getenv" => osGetenv args
  | "os.listdir" => osListdir args
  -- ============================================================
  -- os.path module functions
  -- ============================================================
  | "os.path.join" => osPathJoin args
  | "os.path.exists" => osPathExists args
  | "os.path.isfile" => osPathIsfile args
  | "os.path.isdir" => osPathIsdir args
  | "os.path.dirname" => osPathDirname args
  | "os.path.basename" => osPathBasename args
  | "os.path.abspath" => osPathAbspath args
  | "os.path.splitext" => osPathSplitExt args
  | "os.path.normpath" => osPathNormpath args
  -- ============================================================
  -- time module functions
  -- ============================================================
  | "time.time" => timeTime args
  | "time.monotonic" => timeMonotonic args
  | "time.sleep" => timeSleep args
  -- ============================================================
  -- logging module functions
  -- ============================================================
  | "logging.basicConfig" => loggingBasicConfig args kwargs
  -- ============================================================
  -- signal module stubs
  -- ============================================================
  | "signal.signal" =>
    match args with
    | [_, handler] => return handler
    | _ => return .none
  -- ============================================================
  -- tempfile module stubs
  -- ============================================================
  | "tempfile.mkdtemp" => do
    let suffix ← (IO.rand 100000 999999 : IO Nat)
    return .str s!"/tmp/leanpy_{suffix}"
  | _ => throwNotImplemented s!"builtin '{name}' is not implemented"

end LeanPython.Runtime.Builtins
