import Lython.Runtime.Ops

set_option autoImplicit false

namespace Lython.Runtime.Builtins

open Lython.Runtime
open Lython.Runtime.Ops
open Lython.Interpreter

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

/-- `type(obj)` - returns type name as string (stub; proper type objects in Phase 5). -/
def builtinType (args : List Value) : InterpM Value := do
  match args with
  | [v] => return .str s!"<class '{typeName v}'>"
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

/-- `isinstance(obj, classinfo)` - basic type check by name. -/
def builtinIsinstance (args : List Value) : InterpM Value := do
  match args with
  | [obj, .str className] =>
    return .bool (typeName obj == className)
  | [_obj, .builtin name] =>
    -- isinstance(x, int) where int is the builtin
    let tn := typeName _obj
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
    return .bool isMatch
  | _ => throwTypeError "isinstance() takes 2 arguments"

/-- `hash(obj)` - basic hash. -/
def builtinHash (args : List Value) : InterpM Value := do
  match args with
  | [.int n] => return .int n
  | [.bool b] => return .int (if b then 1 else 0)
  | [.str s] => return .int (hash s).toNat
  | [.none] => return .int 0
  | [.float _f] => return .int 0  -- stub
  | [.tuple _] => return .int 0  -- stub
  | _ => throwTypeError "unhashable type"

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
    | _ => false)
  | _ => throwTypeError "callable() takes exactly one argument"

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
  | _ => throwNotImplemented s!"builtin '{name}' is not implemented"

end Lython.Runtime.Builtins
