import LeanPython.Runtime.Builtins
import LeanPython.Parser.Core
import Std.Data.HashMap
import Std.Data.HashSet

set_option autoImplicit false
set_option maxHeartbeats 400000

namespace LeanPython.Interpreter.Eval

open LeanPython.AST
open LeanPython.Runtime
open LeanPython.Runtime.Ops
open LeanPython.Runtime.Builtins
open LeanPython.Interpreter
open LeanPython.Parser (parse)
open LeanPython.Stdlib.IO
open LeanPython.Stdlib.Hashlib
open LeanPython.Stdlib.Hmac
open LeanPython.Stdlib.Secrets
open LeanPython.Stdlib.Sys
open LeanPython.Stdlib.Os
open LeanPython.Stdlib.Time (timeTime timeMonotonic timeSleep)
open LeanPython.Stdlib.Datetime
open LeanPython.Stdlib.Pathlib
open LeanPython.Stdlib.Logging
open LeanPython.Stdlib.Pydantic
open LeanPython.Lexer (SourceSpan SourcePos)

-- ============================================================
-- Helpers
-- ============================================================

private def constToValue : Constant → Value
  | .none_      => .none
  | .true_      => .bool true
  | .false_     => .bool false
  | .ellipsis   => .ellipsis
  | .int n      => .int n
  | .float_ f   => .float f
  | .imaginary _ => .float 0.0
  | .string s   => .str s
  | .bytes b    => .bytes b

private def normalizeIndex (idx : Int) (len : Nat) : Int :=
  if idx < 0 then idx + len else idx

/-- Erase element at index from array (without proof obligation). -/
private def arrayEraseAt {α : Type} (arr : Array α) (i : Nat) : Array α :=
  (arr.toList.take i ++ arr.toList.drop (i + 1)).toArray

/-- Get string characters as a list (workaround for String.Slice issues). -/
private def strChars (s : String) : List Char := s.toList

/-- Drop n characters from string, returning String (not Slice). -/
private def strDrop (s : String) (n : Nat) : String :=
  String.ofList (s.toList.drop n)

private def dummySpan : SourceSpan :=
  { start := { line := 0, column := 0, offset := 0 }
    stop := { line := 0, column := 0, offset := 0 } }

private def computeSliceIndices (len : Int) (startOpt stopOpt stepOpt : Option Value)
    : InterpM (Int × Int × Int) := do
  let step : Int := match stepOpt with
    | some (.int s) => s
    | some (.none) | none => 1
    | _ => 1
  if step == 0 then throwValueError "slice step cannot be zero"
  let start : Int := match startOpt with
    | some (.int s) => if s < 0 then max 0 (len + s) else min s len
    | some (.none) | none => if step > 0 then 0 else len - 1
    | _ => if step > 0 then 0 else len - 1
  let stop : Int := match stopOpt with
    | some (.int s) => if s < 0 then max 0 (len + s) else min s len
    | some (.none) | none => if step > 0 then len else -1
    | _ => if step > 0 then len else -1
  return (start, stop, step)

private def sliceIndices (start stop step : Int) : List Nat :=
  if step > 0 then
    Id.run do
      let mut result : List Nat := []
      let mut i := start
      while i < stop do
        result := result ++ [i.toNat]
        i := i + step
      return result
  else
    Id.run do
      let mut result : List Nat := []
      let mut i := start
      while i > stop do
        result := result ++ [i.toNat]
        i := i + step
      return result

-- ============================================================
-- Known method names per type (used to validate attribute access)
-- ============================================================

private def knownStrMethods : List String :=
  ["upper", "lower", "strip", "lstrip", "rstrip", "split", "join", "replace",
   "startswith", "endswith", "find", "removeprefix", "removesuffix",
   "isdigit", "isalpha", "encode", "zfill", "format", "count"]

private def knownListMethods : List String :=
  ["append", "extend", "pop", "insert", "remove", "reverse", "clear",
   "copy", "index", "count", "sort"]

private def knownDictMethods : List String :=
  ["get", "keys", "values", "items", "pop", "update", "clear", "copy", "setdefault"]

private def knownSetMethods : List String :=
  ["add", "remove", "discard", "clear", "copy", "union", "intersection",
   "difference", "symmetric_difference", "issubset", "issuperset", "isdisjoint"]

private def knownIntMethods : List String :=
  ["bit_length", "to_bytes"]

private def knownBytesMethods : List String :=
  ["hex", "decode"]

private def knownTupleMethods : List String :=
  ["count", "index"]

-- ============================================================
-- Dunder method name mappings
-- ============================================================

private def binOpToDunder : BinOp → String
  | .add => "__add__"
  | .sub => "__sub__"
  | .mult => "__mul__"
  | .div => "__truediv__"
  | .floorDiv => "__floordiv__"
  | .mod => "__mod__"
  | .pow => "__pow__"
  | .lShift => "__lshift__"
  | .rShift => "__rshift__"
  | .bitOr => "__or__"
  | .bitAnd => "__and__"
  | .bitXor => "__xor__"
  | .matMult => "__matmul__"

private def binOpToRDunder : BinOp → String
  | .add => "__radd__"
  | .sub => "__rsub__"
  | .mult => "__rmul__"
  | .div => "__rtruediv__"
  | .floorDiv => "__rfloordiv__"
  | .mod => "__rmod__"
  | .pow => "__rpow__"
  | .lShift => "__rlshift__"
  | .rShift => "__rrshift__"
  | .bitOr => "__ror__"
  | .bitAnd => "__rand__"
  | .bitXor => "__rxor__"
  | .matMult => "__rmatmul__"

private def cmpOpToDunder : CmpOp → Option String
  | .eq => some "__eq__"
  | .notEq => some "__ne__"
  | .lt => some "__lt__"
  | .ltE => some "__le__"
  | .gt => some "__gt__"
  | .gtE => some "__ge__"
  | .in_ => none
  | .notIn => none
  | .is_ => none
  | .isNot => none

private def unaryOpToDunder : UnaryOp → Option String
  | .uSub => some "__neg__"
  | .uAdd => some "__pos__"
  | .invert => some "__invert__"
  | .not_ => none

-- ============================================================
-- C3 MRO linearization (outside mutual block — only uses heap ops)
-- ============================================================

/-- C3 MRO merge: repeatedly pick the first head that doesn't appear
    in the tail of any other non-empty list. -/
private partial def c3Merge (lists : List (List Value)) : InterpM (List Value) := do
  let nonEmpty := lists.filter (!·.isEmpty)
  if nonEmpty.isEmpty then return []
  -- Find a good head
  let mut goodHead : Option Value := none
  for l in nonEmpty do
    let candidate := l.head!
    let inTail := nonEmpty.any fun other =>
      match other with
      | _ :: rest => rest.any fun v => Value.beq v candidate
      | [] => false
    if !inTail then
      goodHead := some candidate
      break
  match goodHead with
  | none => throwTypeError "Cannot create a consistent method resolution order (MRO)"
  | some head =>
    let cleaned := nonEmpty.map fun l =>
      if l.isEmpty then []
      else if Value.beq l.head! head then l.tail!
      else l
    let rest ← c3Merge cleaned
    return head :: rest

/-- Compute C3 MRO for a class with given bases. -/
private partial def computeC3Mro (selfClass : Value) (bases : Array Value)
    : InterpM (Array Value) := do
  if bases.isEmpty then return #[selfClass]
  let mut baseMros : List (List Value) := []
  for base in bases do
    match base with
    | .classObj bref => do
      let bcd ← heapGetClassData bref
      baseMros := baseMros ++ [bcd.mro.toList]
    | _ => pure ()
  baseMros := baseMros ++ [bases.toList]
  let merged ← c3Merge baseMros
  return (#[selfClass] ++ merged.toArray)

/-- Convert a runtime Value back to an AST Constant for default parameters. -/
private def valueToConstant : Value → Constant
  | .int n => .int n
  | .float f => .float_ f
  | .str s => .string s
  | .bool true => .true_
  | .bool false => .false_
  | .none => .none_
  | _ => .none_

-- ============================================================
-- Mutual block: evalExpr, execStmt, and helpers
-- ============================================================

-- ============================================================
-- Module system helpers (outside mutual block)
-- ============================================================

/-- Check if a file exists on the filesystem. -/
private def fileExists (path : System.FilePath) : IO Bool := do
  try
    let _ ← IO.FS.readFile path
    return true
  catch _ =>
    return false

/-- Resolve a dotted module name to a file path.
    Returns (absolutePath, isPackage) or none if not found. -/
private def resolveModulePath (moduleName : String) (searchPaths : Array String)
    : IO (Option (String × Bool)) := do
  let parts := moduleName.splitOn "."
  for base in searchPaths do
    let basePath := System.FilePath.mk base
    -- Build the path from parts
    let mut dirPath := basePath
    for p in parts do
      dirPath := dirPath / p
    -- Try as package: dir/__init__.py
    let initPath := dirPath / "__init__.py"
    if ← fileExists initPath then
      return some (initPath.toString, true)
    -- Try as module: dir.py (go up one level and add .py)
    let parentPath := dirPath.parent.getD basePath
    let lastPart := parts.getLast!
    let modPath := parentPath / (lastPart ++ ".py")
    if ← fileExists modPath then
      return some (modPath.toString, false)
  return none

/-- Resolve a relative import to an absolute module name. -/
private def resolveRelativeImport (currentPackage : Option String) (level : Nat)
    (moduleName : Option String) : Except String String :=
  match currentPackage with
  | none => .error "attempted relative import with no known parent package"
  | some pkg =>
    let parts := pkg.splitOn "."
    -- level 1 = current package, level 2 = parent, etc.
    let goUp := level - 1
    if goUp > parts.length then
      .error "attempted relative import beyond top-level package"
    else
      let baseParts := parts.take (parts.length - goUp)
      let base := ".".intercalate baseParts
      match moduleName with
      | none => if base.isEmpty then .error "empty module name" else .ok base
      | some mn =>
        if base.isEmpty then .ok mn
        else .ok (base ++ "." ++ mn)

mutual

-- ============================================================
-- Dunder method lookup and dispatch
-- ============================================================

/-- Look up a dunder method on an instance's class MRO.
    Returns the raw function Value and the defining class, or none. -/
partial def lookupDunder (inst : Value) (name : String) : InterpM (Option (Value × Value)) := do
  let cls ← match inst with
    | .instance iref => do
      let id_ ← heapGetInstanceData iref
      pure id_.cls
    | _ => pure Value.none
  match cls with
  | .classObj cref => do
    let cd ← heapGetClassData cref
    for mroEntry in cd.mro do
      match mroEntry with
      | .classObj mref => do
        let mcd ← heapGetClassData mref
        if let some fn := mcd.ns[name]? then
          return some (fn, mroEntry)
      | _ => pure ()
    return none
  | _ => return none

/-- Call a dunder method on an instance. Returns none if the dunder is not defined. -/
partial def callDunder (inst : Value) (name : String) (args : List Value) : InterpM (Option Value) := do
  match ← lookupDunder inst name with
  | some (fn, definingCls) =>
    match fn with
    | .function fref => do
      let fd ← heapGetFunc fref
      let scope ← bindFuncParams fd.params (inst :: args) [] fd.defaults fd.kwDefaults
      let scopeWithClass := scope.insert "__class__" definingCls
      some <$> callRegularFunc fd scopeWithClass
    | _ => some <$> callValueDispatch fn (inst :: args) []
  | none => return none

partial def evalExpr (e : Expr) : InterpM Value := do
  match e with
  | .name n _ => lookupVariable n
  | .constant c _ => return (constToValue c)

  | .binOp left op right _ => do
    let l ← evalExpr left
    let r ← evalExpr right
    -- Try dunder dispatch for instances
    match l with
    | .instance _ =>
      match ← callDunder l (binOpToDunder op) [r] with
      | some result => return result
      | none =>
        -- Fallback: try callBoundMethod for builtin instance types
        let bmResult ← try
          let v ← callBoundMethod l (binOpToDunder op) [r]
          pure (some v)
        catch _ => pure none
        match bmResult with
        | some result => return result
        | none =>
          -- Try reflected operator on right
          match r with
          | .instance _ =>
            match ← callDunder r (binOpToRDunder op) [l] with
            | some result => return result
            | none => evalBinOp op l r
          | _ => evalBinOp op l r
    | _ =>
      match r with
      | .instance _ =>
        match ← callDunder r (binOpToRDunder op) [l] with
        | some result => return result
        | none => evalBinOp op l r
      | _ => evalBinOp op l r

  | .unaryOp op operand _ => do
    let v ← evalExpr operand
    match v with
    | .instance _ =>
      match unaryOpToDunder op with
      | some dunName =>
        match ← callDunder v dunName [] with
        | some result => return result
        | none => evalUnaryOp op v
      | none => evalUnaryOp op v
    | _ => evalUnaryOp op v

  | .boolOp op exprs _ => do
    match exprs with
    | [] => return .none
    | [single] => evalExpr single
    | first :: rest => do
      let v ← evalExpr first
      evalBoolOp op [v] evalExpr rest

  | .compare left comparators _ => do
    let mut leftVal ← evalExpr left
    for (op, rightExpr) in comparators do
      let rightVal ← evalExpr rightExpr
      let result ← match leftVal with
        | .instance _ =>
          -- Try __contains__ for `in` operator (on the right/container)
          if op == .in_ || op == .notIn then
            match rightVal with
            | .instance _ =>
              match ← callDunder rightVal "__contains__" [leftVal] with
              | some v => do
                let b ← isTruthy v
                pure (if op == .notIn then !b else b)
              | none => evalCmpOp op leftVal rightVal
            | _ => evalCmpOp op leftVal rightVal
          else
            match cmpOpToDunder op with
            | some dunName =>
              match ← callDunder leftVal dunName [rightVal] with
              | some v => isTruthy v
              | none => evalCmpOp op leftVal rightVal
            | none => evalCmpOp op leftVal rightVal
        | _ =>
          -- Check if right operand is an instance for `in` operator
          if op == .in_ || op == .notIn then
            match rightVal with
            | .instance _ =>
              match ← callDunder rightVal "__contains__" [leftVal] with
              | some v => do
                let b ← isTruthy v
                pure (if op == .notIn then !b else b)
              | none => evalCmpOp op leftVal rightVal
            | _ => evalCmpOp op leftVal rightVal
          else
            evalCmpOp op leftVal rightVal
      if !result then return .bool false
      leftVal := rightVal
    return .bool true

  | .call func posArgs kwArgs _ => do
    let callee ← evalExpr func
    let argVals ← evalArgList posArgs
    let kwargVals ← evalKwargList kwArgs
    callValueDispatch callee argVals kwargVals

  | .attribute obj attrName _ => do
    let objVal ← evalExpr obj
    getAttributeValue objVal attrName

  | .subscript obj idx _ => do
    let objVal ← evalExpr obj
    let idxVal ← evalExpr idx
    evalSubscriptValue objVal idxVal

  | .ifExp test body orelse _ => do
    if ← isTruthy (← evalExpr test) then evalExpr body
    else evalExpr orelse

  | .namedExpr target value _ => do
    let v ← evalExpr value
    match target with
    | .name n _ => setVariable n v
    | _ => throwRuntimeError (.runtimeError "invalid walrus target")
    return v

  | .lambda_ params body _ => do
    let defaults ← params.defaults.mapM evalExpr
    let kwDefaults ← params.kwDefaults.mapM fun
      | some expr => do return some (← evalExpr expr)
      | none => return none
    let st ← get
    allocFunc (FuncData.mk "<lambda>" params [.return_ (some body) dummySpan] defaults.toArray kwDefaults.toArray st.localScopes.toArray false)

  | .tuple elems _ => do
    let vals ← elems.mapM evalExpr
    return .tuple vals.toArray

  | .list_ elems _ => do
    let vals ← elems.mapM evalExpr
    allocList vals.toArray

  | .set_ elems _ => do
    let vals ← elems.mapM evalExpr
    allocSet vals.toArray

  | .dict pairs _ => do
    let mut result : Array (Value × Value) := #[]
    for (keyOpt, valExpr) in pairs do
      let v ← evalExpr valExpr
      match keyOpt with
      | some keyExpr => do
        let k ← evalExpr keyExpr
        result := result.push (k, v)
      | none =>
        -- **kwargs dict unpacking
        match v with
        | .dict ref => do
          let entries ← heapGetDict ref
          result := result ++ entries
        | _ => throwTypeError "cannot unpack non-dict in dict literal"
    allocDict result

  | .slice lower upper step _ => do
    let l ← lower.mapM evalExpr
    let u ← upper.mapM evalExpr
    let s ← step.mapM evalExpr
    -- Encode slice as tuple for subscript to recognize
    return .tuple #[l.getD .none, u.getD .none, s.getD .none]

  | .fstring parts _ => do
    let mut result := ""
    for part in parts do
      let s ← evalExpr part
      result := result ++ (← valueToStr s)
    return .str result

  | .formattedValue expr _conv _formatSpec _ => do
    let v ← evalExpr expr
    return .str (← valueToStr v)

  | .starred inner _ => evalExpr inner

  | .listComp elt generators _ => do
    let mut results : Array Value := #[]
    results ← evalCompGen generators (do return (← evalExpr elt)) results
    allocList results

  | .setComp elt generators _ => do
    let mut results : Array Value := #[]
    results ← evalCompGen generators (do return (← evalExpr elt)) results
    allocSet results

  | .dictComp key value generators _ => do
    let mut results : Array (Value × Value) := #[]
    results ← evalDictCompGen generators (do
      let k ← evalExpr key
      let v ← evalExpr value
      return (k, v)) results
    allocDict results

  | .generatorExp elt generators _ => do
    let mut results : Array Value := #[]
    results ← evalCompGen generators (do return (← evalExpr elt)) results
    allocGenerator results

  | .await_ _ _ => throwNotImplemented "await"
  | .yield_ exprOpt _ => do
    let v ← match exprOpt with
      | some expr => evalExpr expr
      | none => pure .none
    let st ← get
    match st.yieldAccumulator with
    | some acc => set { st with yieldAccumulator := some (acc.push v) }
    | none => throwRuntimeError (.runtimeError "yield outside generator function")
    return .none
  | .yieldFrom iterExpr _ => do
    let iterVal ← evalExpr iterExpr
    let items ← iterValues iterVal
    let st ← get
    match st.yieldAccumulator with
    | some acc => set { st with yieldAccumulator := some (acc ++ items) }
    | none => throwRuntimeError (.runtimeError "yield from outside generator function")
    return .none

-- Evaluate positional args, handling *args unpacking
partial def evalArgList (args : List Expr) : InterpM (List Value) := do
  let mut result : List Value := []
  for arg in args do
    match arg with
    | .starred inner _ => do
      let v ← evalExpr inner
      let items ← iterValues v
      result := result ++ items.toList
    | _ => result := result ++ [← evalExpr arg]
  return result

-- Evaluate keyword args
partial def evalKwargList (kwargs : List CallKeyword) : InterpM (List (String × Value)) := do
  let mut result : List (String × Value) := []
  for kw in kwargs do
    let v ← evalExpr kw.value
    match kw.name with
    | some name => result := result ++ [(name, v)]
    | none =>
      match v with
      | .dict ref => do
        let entries ← heapGetDict ref
        for (k, val) in entries do
          match k with
          | .str s => result := result ++ [(s, val)]
          | _ => throwTypeError "keywords must be strings"
      | _ => throwTypeError "argument after ** must be a mapping"
  return result

-- Dispatch a call
partial def callValueDispatch (callee : Value) (args : List Value)
    (kwargs : List (String × Value)) : InterpM Value := do
  match callee with
  | .builtin name => do
    -- Handle builtins that need callValueDispatch or dunder dispatch
    match name with
    | "map" => builtinMap args
    | "filter" => builtinFilter args
    | "functools.reduce" => builtinFunctoolsReduce args
    | "itertools.accumulate" => builtinItertoolsAccumulate args kwargs
    | "collections.defaultdict" => do
      -- defaultdict(factory) or defaultdict(factory, pairs)
      let (factory, initPairs) ← match args with
        | [] => pure (Value.none, #[])
        | [f] => pure (f, #[])
        | [f, init] => do
          let items ← iterValues init
          let mut pairs : Array (Value × Value) := #[]
          for item in items do
            let kv ← iterValues item
            if kv.size >= 2 then pairs := pairs.push (kv[0]!, kv[1]!)
          pure (f, pairs)
        | _ => throwTypeError "defaultdict() takes at most 2 positional arguments"
      let mut pairs := initPairs
      for (k, v) in kwargs do
        pairs := pairs.push (.str k, v)
      let dictRef ← heapAlloc (.dictObj pairs)
      -- Register factory for this dict ref
      modify fun st => { st with defaultFactories := st.defaultFactories.insert dictRef factory }
      return .dict dictRef
    | "collections.OrderedDict" => do
      -- OrderedDict is just a dict (Python 3.7+ dicts preserve order)
      let mut pairs : Array (Value × Value) := #[]
      for (k, v) in kwargs do
        pairs := pairs.push (.str k, v)
      allocDict pairs
    | "collections.deque" => do
      -- deque(iterable) — create a list from iterable
      match args with
      | [] => allocList #[]
      | [iter] => do
        let items ← iterValues iter
        allocList items
      | _ => throwTypeError "deque() takes at most 1 argument"
    | "operator.itemgetter" => do
      -- itemgetter(key) returns a callable
      match args with
      | [.str key] => return .builtin s!"operator.itemgetter_s:{key}"
      | [.int key] => return .builtin s!"operator.itemgetter_i:{key}"
      | _ => throwTypeError "operator.itemgetter() requires a string or int key"
    | "operator.attrgetter" => do
      match args with
      | [.str name] => return .builtin s!"operator.attrgetter_inst:{name}"
      | _ => throwTypeError "operator.attrgetter() requires a string argument"
    | "hasattr" => builtinHasattr args
    | "getattr" => builtinGetattr args
    | "setattr" => builtinSetattr args
    | "staticmethod" => match args with
      | [fn] => return .staticMethod fn
      | _ => throwTypeError "staticmethod() takes exactly one argument"
    | "classmethod" => match args with
      | [fn] => return .classMethod fn
      | _ => throwTypeError "classmethod() takes exactly one argument"
    | "property" => match args with
      | [] => return .property .none none none
      | [fn] => return .property fn none none
      | _ => throwTypeError "property() takes at most one argument"
    | "str" => match args with
      | [inst@(.instance _)] =>
        match ← callDunder inst "__str__" [] with
        | some v => return v
        | none =>
          -- Fallback: try callBoundMethod for builtin instance types
          let r ← try
            let v ← callBoundMethod inst "__str__" []
            pure (some v)
          catch _ => pure none
          match r with
          | some v => return v
          | none => callBuiltin name args kwargs
      | _ => callBuiltin name args kwargs
    | "repr" => match args with
      | [inst@(.instance _)] =>
        match ← callDunder inst "__repr__" [] with
        | some v => return v
        | none =>
          let r ← try
            let v ← callBoundMethod inst "__repr__" []
            pure (some v)
          catch _ => pure none
          match r with
          | some v => return v
          | none => callBuiltin name args kwargs
      | _ => callBuiltin name args kwargs
    | "len" => match args with
      | [inst@(.instance _)] =>
        match ← callDunder inst "__len__" [] with
        | some v => return v
        | none => callBuiltin name args kwargs
      | _ => callBuiltin name args kwargs
    | "hash" => match args with
      | [inst@(.instance _)] =>
        match ← callDunder inst "__hash__" [] with
        | some v => return v
        | none => callBuiltin name args kwargs
      | _ => callBuiltin name args kwargs
    | "bool" => match args with
      | [inst@(.instance _)] =>
        match ← callDunder inst "__bool__" [] with
        | some v => return v
        | none => callBuiltin name args kwargs
      | _ => callBuiltin name args kwargs
    | "iter" => match args with
      | [inst@(.instance _)] =>
        match ← callDunder inst "__iter__" [] with
        | some v => return v
        | none => callBuiltin name args kwargs
      | _ => callBuiltin name args kwargs
    | "next" => match args with
      | [inst@(.instance _)] =>
        match ← callDunder inst "__next__" [] with
        | some v => return v
        | none => callBuiltin name args kwargs
      | _ => callBuiltin name args kwargs
    | "print" => do
      -- Convert instance args to strings via __str__ first
      let mut args' : List Value := []
      for arg in args do
        match arg with
        | .instance _ =>
          match ← callDunder arg "__str__" [] with
          | some (.str s) => args' := args' ++ [.str s]
          | some v => args' := args' ++ [v]
          | none =>
            -- Fallback: try callBoundMethod for builtin instance types
            let strResult ← try
              let v ← callBoundMethod arg "__str__" []
              pure (some v)
            catch _ => pure none
            match strResult with
            | some (.str s) => args' := args' ++ [.str s]
            | _ => args' := args' ++ [arg]
        | other => args' := args' ++ [other]
      callBuiltin name args' kwargs
    | "pydantic.ConfigDict" => do
        -- ConfigDict(frozen=True, extra="forbid", ...) → dict with config values
        let mut pairs : Array (Value × Value) := #[]
        for (k, v) in kwargs do
          pairs := pairs.push (.str k, v)
        allocDict pairs
    | "pydantic.Field" => do
        -- Field(default=..., alias=...) → dict with field metadata
        let mut pairs : Array (Value × Value) := #[]
        for (k, v) in kwargs do
          pairs := pairs.push (.str k, v)
        allocDict pairs
    | "pydantic.field_validator" => do
        -- field_validator("field1", "field2", ..., mode="after")
        -- Returns a parametric decorator builtin
        let fieldNames := args.filterMap fun | .str s => some s | _ => none
        let mode := match kwargs.find? (fun (k, _) => k == "mode") with
          | some (_, .str m) => m | _ => "after"
        let fnStr := ",".intercalate fieldNames
        return .builtin s!"pydantic.fv_dec:{mode}:{fnStr}"
    | "pydantic.model_validator" => do
        -- model_validator(mode="before"|"after")
        let mode := match kwargs.find? (fun (k, _) => k == "mode") with
          | some (_, .str m) => m | _ => "after"
        return .builtin s!"pydantic.mv_dec:{mode}"
    | "pydantic.field_serializer" => do
        -- field_serializer("field1", ..., when_used="always"|"json")
        let fieldNames := args.filterMap fun | .str s => some s | _ => none
        let whenUsed := match kwargs.find? (fun (k, _) => k == "when_used") with
          | some (_, .str w) => w | _ => "always"
        let fnStr := ",".intercalate fieldNames
        return .builtin s!"pydantic.fs_dec:{whenUsed}:{fnStr}"
    | "pydantic.model_serializer" => do
        -- model_serializer(mode="plain"|"wrap", when_used="always"|"json")
        let mode := match kwargs.find? (fun (k, _) => k == "mode") with
          | some (_, .str m) => m | _ => "plain"
        let whenUsed := match kwargs.find? (fun (k, _) => k == "when_used") with
          | some (_, .str w) => w | _ => "always"
        return .builtin s!"pydantic.ms_dec:{mode}:{whenUsed}"
    | "dataclass" => match args with
      | [cls@(.classObj _)] => applyDataclass cls false false
      | [] =>
        -- dataclass() with keyword args returns a decorator
        let frozen := kwargs.any fun (k, v) => k == "frozen" && Value.beq v (.bool true)
        let slotsKw := kwargs.any fun (k, v) => k == "slots" && Value.beq v (.bool true)
        -- Return a lambda-like builtin that captures frozen/slots config
        let tag := if frozen && slotsKw then "dataclass_frozen_slots"
          else if frozen then "dataclass_frozen"
          else if slotsKw then "dataclass_slots"
          else "dataclass"
        return .builtin tag
      | _ => throwTypeError "dataclass() takes a class or keyword arguments"
    | "dataclass_frozen" => match args with
      | [cls@(.classObj _)] => applyDataclass cls true false
      | _ => throwTypeError "dataclass() takes a class"
    | "dataclass_frozen_slots" => match args with
      | [cls@(.classObj _)] => applyDataclass cls true true
      | _ => throwTypeError "dataclass() takes a class"
    | "dataclass_slots" => match args with
      | [cls@(.classObj _)] => applyDataclass cls false true
      | _ => throwTypeError "dataclass() takes a class"
    | _ =>
      -- Handle prefix-matched builtins (operator.itemgetter instances, etc.)
      if name.startsWith "operator.itemgetter_s:" then do
        let key := String.ofList (name.toList.drop "operator.itemgetter_s:".length)
        match args with
        | [obj] => evalSubscriptValue obj (.str key)
        | _ => throwTypeError "itemgetter object takes exactly 1 argument"
      else if name.startsWith "operator.itemgetter_i:" then do
        let keyStr := String.ofList (name.toList.drop "operator.itemgetter_i:".length)
        let key := keyStr.toInt?.getD 0
        match args with
        | [obj] => evalSubscriptValue obj (.int key)
        | _ => throwTypeError "itemgetter object takes exactly 1 argument"
      else if name.startsWith "operator.attrgetter_inst:" then do
        let attrName := String.ofList (name.toList.drop "operator.attrgetter_inst:".length)
        match args with
        | [obj] => getAttributeValue obj attrName
        | _ => throwTypeError "attrgetter object takes exactly 1 argument"
      else if name.startsWith "pydantic.fv_dec:" then do
        -- Parametric field_validator decorator: wraps fn in marker tuple
        let rest := String.ofList (name.toList.drop "pydantic.fv_dec:".length)
        let parts := rest.splitOn ":"
        let (mode, fnStr) := match parts with
          | [m, f] => (m, f) | [m] => (m, "") | _ => ("after", "")
        let fieldNames := (fnStr.splitOn ",").filter (· ≠ "")
        let fnArr := fieldNames.map (fun n => Value.str n) |>.toArray
        let fnListRef ← heapAlloc (.listObj fnArr)
        match args with
        | [fn] =>
          -- Unwrap classmethod if present to get the raw function
          let rawFn := match fn with | .classMethod inner => inner | other => other
          return .tuple #[
            .str "__pydantic_field_validator__", .str mode, .list fnListRef, rawFn]
        | _ => throwTypeError "field_validator decorator takes exactly one argument"
      else if name.startsWith "pydantic.mv_dec:" then do
        -- Parametric model_validator decorator: wraps fn in marker tuple
        let mode := String.ofList (name.toList.drop "pydantic.mv_dec:".length)
        match args with
        | [fn] =>
          let rawFn := match fn with | .classMethod inner => inner | other => other
          return .tuple #[
            .str "__pydantic_model_validator__", .str mode, rawFn]
        | _ => throwTypeError "model_validator decorator takes exactly one argument"
      else if name.startsWith "pydantic.fs_dec:" then do
        -- Parametric field_serializer decorator: wraps fn in marker tuple
        let rest := String.ofList (name.toList.drop "pydantic.fs_dec:".length)
        let parts := rest.splitOn ":"
        let (whenUsed, fnStr) := match parts with
          | [w, f] => (w, f) | [w] => (w, "") | _ => ("always", "")
        let fieldNames := (fnStr.splitOn ",").filter (· ≠ "")
        let fnArr := fieldNames.map (fun n => Value.str n) |>.toArray
        let fnListRef ← heapAlloc (.listObj fnArr)
        match args with
        | [fn] =>
          return .tuple #[
            .str "__pydantic_field_serializer__", .str whenUsed, .list fnListRef, fn]
        | _ => throwTypeError "field_serializer decorator takes exactly one argument"
      else if name.startsWith "pydantic.ms_dec:" then do
        -- Parametric model_serializer decorator: wraps fn in marker tuple
        let rest := String.ofList (name.toList.drop "pydantic.ms_dec:".length)
        let parts := rest.splitOn ":"
        let (mode, whenUsed) := match parts with
          | [m, w] => (m, w) | [m] => (m, "always") | _ => ("plain", "always")
        match args with
        | [fn] =>
          return .tuple #[
            .str "__pydantic_model_serializer__", .str mode, .str whenUsed, fn]
        | _ => throwTypeError "model_serializer decorator takes exactly one argument"
      -- ============================================================
      -- core_schema builtin functions
      -- ============================================================
      else if name == "core_schema.union_schema" then do
        -- union_schema([schema1, schema2, ...], serialization=...)
        let schemas := match args with | [s] => s | _ => Value.none
        let serialization := match kwargs.find? (fun (k, _) => k == "serialization") with
          | some (_, v) => v | _ => Value.none
        let mut pairs : Array (Value × Value) := #[]
        pairs := pairs.push (.str "type", .str "union")
        pairs := pairs.push (.str "choices", schemas)
        if !Value.beq serialization .none then
          pairs := pairs.push (.str "serialization", serialization)
        allocDict pairs
      else if name == "core_schema.is_instance_schema" then do
        let cls := match args with | [c] => c | _ => Value.none
        let mut pairs : Array (Value × Value) := #[]
        pairs := pairs.push (.str "type", .str "is-instance")
        pairs := pairs.push (.str "cls", cls)
        allocDict pairs
      else if name == "core_schema.chain_schema" then do
        let schemas := match args with | [s] => s | _ => Value.none
        let mut pairs : Array (Value × Value) := #[]
        pairs := pairs.push (.str "type", .str "chain")
        pairs := pairs.push (.str "steps", schemas)
        allocDict pairs
      else if name == "core_schema.int_schema" then do
        let mut pairs : Array (Value × Value) := #[]
        pairs := pairs.push (.str "type", .str "int")
        for (k, v) in kwargs do
          pairs := pairs.push (.str k, v)
        allocDict pairs
      else if name == "core_schema.bool_schema" then do
        let mut pairs : Array (Value × Value) := #[]
        pairs := pairs.push (.str "type", .str "bool")
        for (k, v) in kwargs do
          pairs := pairs.push (.str k, v)
        allocDict pairs
      else if name == "core_schema.bytes_schema" then do
        let mut pairs : Array (Value × Value) := #[]
        pairs := pairs.push (.str "type", .str "bytes")
        for (k, v) in kwargs do
          pairs := pairs.push (.str k, v)
        allocDict pairs
      else if name == "core_schema.no_info_plain_validator_function" then do
        let func := match args with | [f] => f | _ => Value.none
        let serialization := match kwargs.find? (fun (k, _) => k == "serialization") with
          | some (_, v) => v | _ => Value.none
        let mut pairs : Array (Value × Value) := #[]
        pairs := pairs.push (.str "type", .str "no-info-plain-validator")
        pairs := pairs.push (.str "function", func)
        if !Value.beq serialization .none then
          pairs := pairs.push (.str "serialization", serialization)
        allocDict pairs
      else if name == "core_schema.plain_serializer_function_ser_schema" then do
        let func := match args with | [f] => f | _ => Value.none
        let mut pairs : Array (Value × Value) := #[]
        pairs := pairs.push (.str "type", .str "plain-serializer")
        pairs := pairs.push (.str "function", func)
        allocDict pairs
      else if name == "pydantic.alias_generators.to_camel" then do
        -- to_camel("snake_case") → "snakeCase"
        match args with
        | [.str s] =>
          let parts := s.splitOn "_"
          let result := match parts with
          | [] => ""
          | first :: rest =>
            let capitalized := rest.map fun part =>
              if part.isEmpty then ""
              else
                let firstChar := part.toList.head!
                let restChars := String.ofList (part.toList.drop 1)
                s!"{firstChar.toUpper}{restChars}"
            first ++ String.join capitalized
          return .str result
        | _ => throwTypeError "to_camel() takes exactly one string argument"
      else if name == "pydantic_core.CoreSchema" || name == "pydantic.GetCoreSchemaHandler" then
        -- Stub type: just return None (used only for type annotations)
        return .none
      else if name == "io.BytesIO" then do
        -- Construct a BytesIO instance with _buffer and _pos attrs
        let initBuf := match args with
          | [.bytes b] => b
          | [] => ByteArray.empty
          | _ => ByteArray.empty
        let mut attrs : Std.HashMap String Value := {}
        attrs := attrs.insert "_buffer" (.bytes initBuf)
        attrs := attrs.insert "_pos" (.int 0)
        -- Create a class object for BytesIO (lightweight)
        let cls ← allocClassObj {
          name := "BytesIO", bases := #[], mro := #[], ns := {}, slots := none }
        match cls with
        | .classObj cref => heapSetClassData cref {
            name := "BytesIO", bases := #[], mro := #[cls], ns := {}, slots := none }
        | _ => pure ()
        let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
        return .instance instRef
      else if name == "io.StringIO" then do
        let initBuf := match args with
          | [.str s] => s
          | [] => ""
          | _ => ""
        let mut attrs : Std.HashMap String Value := {}
        attrs := attrs.insert "_buffer" (.str initBuf)
        attrs := attrs.insert "_pos" (.int 0)
        let cls ← allocClassObj {
          name := "StringIO", bases := #[], mro := #[], ns := {}, slots := none }
        match cls with
        | .classObj cref => heapSetClassData cref {
            name := "StringIO", bases := #[], mro := #[cls], ns := {}, slots := none }
        | _ => pure ()
        let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
        return .instance instRef
      else if name == "hashlib.sha256" then do
        let initBuf := match args with
          | [.bytes b] => b
          | [] => ByteArray.empty
          | _ => ByteArray.empty
        let mut attrs : Std.HashMap String Value := {}
        attrs := attrs.insert "_buffer" (.bytes initBuf)
        attrs := attrs.insert "_name" (.str "sha256")
        attrs := attrs.insert "_digest_size" (.int 32)
        attrs := attrs.insert "_is_shake" (.bool false)
        attrs := attrs.insert "digest_size" (.int 32)
        attrs := attrs.insert "block_size" (.int 64)
        attrs := attrs.insert "name" (.str "sha256")
        let cls ← allocClassObj {
          name := "SHA256Hash", bases := #[], mro := #[], ns := {}, slots := none }
        match cls with
        | .classObj cref => heapSetClassData cref {
            name := "SHA256Hash", bases := #[], mro := #[cls], ns := {}, slots := none }
        | _ => pure ()
        let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
        return .instance instRef
      else if name == "hashlib.shake_128" then do
        let initBuf := match args with
          | [.bytes b] => b
          | [] => ByteArray.empty
          | _ => ByteArray.empty
        let mut attrs : Std.HashMap String Value := {}
        attrs := attrs.insert "_buffer" (.bytes initBuf)
        attrs := attrs.insert "_name" (.str "shake_128")
        attrs := attrs.insert "_digest_size" (.int 0)
        attrs := attrs.insert "_is_shake" (.bool true)
        attrs := attrs.insert "digest_size" (.int 0)
        attrs := attrs.insert "block_size" (.int 168)
        attrs := attrs.insert "name" (.str "shake_128")
        let cls ← allocClassObj {
          name := "SHAKE128Hash", bases := #[], mro := #[], ns := {}, slots := none }
        match cls with
        | .classObj cref => heapSetClassData cref {
            name := "SHAKE128Hash", bases := #[], mro := #[cls], ns := {}, slots := none }
        | _ => pure ()
        let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
        return .instance instRef
      else if name == "hashlib.new" then do
        match args with
        | (.str algoName) :: rest =>
          let data := match rest with
            | [.bytes b] => b
            | _ => ByteArray.empty
          let targetName := match algoName with
            | "sha256" => "hashlib.sha256"
            | "shake_128" => "hashlib.shake_128"
            | _ => ""
          if targetName == "" then
            throwValueError s!"unsupported hash type {algoName}"
          else
            callValueDispatch (.builtin targetName) [.bytes data] kwargs
        | _ => throwTypeError "hashlib.new() requires algorithm name as first argument"
      else if name == "hmac.new" then do
        let key := match args with
          | (.bytes k) :: _ => k
          | _ => ByteArray.empty
        let msg := match args with
          | _ :: (.bytes m) :: _ => m
          | _ => ByteArray.empty
        let mut attrs : Std.HashMap String Value := {}
        attrs := attrs.insert "_key" (.bytes key)
        attrs := attrs.insert "_msg" (.bytes msg)
        attrs := attrs.insert "_digestmod" (.str "sha256")
        let cls ← allocClassObj {
          name := "HMAC", bases := #[], mro := #[], ns := {}, slots := none }
        match cls with
        | .classObj cref => heapSetClassData cref {
            name := "HMAC", bases := #[], mro := #[cls], ns := {}, slots := none }
        | _ => pure ()
        let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
        return .instance instRef
      else if name == "hmac.digest" then do
        match args with
        | [.bytes key, .bytes msg, _] =>
          return .bytes (hmacSha256 key msg)
        | _ => throwTypeError "hmac.digest() requires (key, msg, digest)"
      else if name == "secrets.token_bytes" then
        tokenBytes args
      else if name == "secrets.randbelow" then
        randbelow args
      -- ============================================================
      -- datetime module constructors
      -- ============================================================
      else if name == "datetime.datetime" then do
        -- datetime(year, month, day[, hour[, minute[, second[, microsecond]]]])
        let mut attrs : Std.HashMap String Value := {}
        attrs := attrs.insert "_year"        (args.getD 0 (.int 1970))
        attrs := attrs.insert "_month"       (args.getD 1 (.int 1))
        attrs := attrs.insert "_day"         (args.getD 2 (.int 1))
        attrs := attrs.insert "_hour"        (args.getD 3 (.int 0))
        attrs := attrs.insert "_minute"      (args.getD 4 (.int 0))
        attrs := attrs.insert "_second"      (args.getD 5 (.int 0))
        attrs := attrs.insert "_microsecond" (args.getD 6 (.int 0))
        -- Also set kwargs
        for (k, v) in kwargs do
          attrs := attrs.insert s!"_{k}" v
        let cls ← allocClassObj {
          name := "Datetime", bases := #[], mro := #[], ns := {}, slots := none }
        match cls with
        | .classObj cref => heapSetClassData cref {
            name := "Datetime", bases := #[], mro := #[cls], ns := {}, slots := none }
        | _ => pure ()
        let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
        return .instance instRef
      else if name == "datetime.timedelta" then do
        -- timedelta(days=0, seconds=0, microseconds=0, ...)
        let mut days : Int := 0
        let mut seconds : Int := 0
        let mut microseconds : Int := 0
        -- Positional args
        if args.length >= 1 then
          match args.getD 0 .none with | .int n => days := n | .float f => days := f.toUInt64.toNat | _ => pure ()
        if args.length >= 2 then
          match args.getD 1 .none with | .int n => seconds := n | .float f => seconds := f.toUInt64.toNat | _ => pure ()
        if args.length >= 3 then
          match args.getD 2 .none with | .int n => microseconds := n | _ => pure ()
        -- Kwargs override
        for (k, v) in kwargs do
          match k, v with
          | "days", .int n => days := n
          | "days", .float f => days := f.toUInt64.toNat
          | "seconds", .int n => seconds := n
          | "seconds", .float f => seconds := f.toUInt64.toNat
          | "microseconds", .int n => microseconds := n
          | "hours", .int n => seconds := seconds + n * 3600
          | "hours", .float f => seconds := seconds + (f * 3600.0).toUInt64.toNat
          | "minutes", .int n => seconds := seconds + n * 60
          | "minutes", .float f => seconds := seconds + (f * 60.0).toUInt64.toNat
          | "weeks", .int n => days := days + n * 7
          | _, _ => pure ()
        -- Normalize: seconds overflow into days
        days := days + seconds / 86400
        seconds := seconds % 86400
        let mut attrs : Std.HashMap String Value := {}
        attrs := attrs.insert "_days"         (.int days)
        attrs := attrs.insert "_seconds"      (.int seconds)
        attrs := attrs.insert "_microseconds" (.int microseconds)
        attrs := attrs.insert "days"          (.int days)
        attrs := attrs.insert "seconds"       (.int seconds)
        attrs := attrs.insert "microseconds"  (.int microseconds)
        let cls ← allocClassObj {
          name := "Timedelta", bases := #[], mro := #[], ns := {}, slots := none }
        match cls with
        | .classObj cref => heapSetClassData cref {
            name := "Timedelta", bases := #[], mro := #[cls], ns := {}, slots := none }
        | _ => pure ()
        let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
        return .instance instRef
      else if name == "datetime.timezone" then do
        -- timezone(offset) or timezone.utc
        let mut attrs : Std.HashMap String Value := {}
        attrs := attrs.insert "_name" (.str "UTC")
        attrs := attrs.insert "_offset" (.int 0)
        let cls ← allocClassObj {
          name := "Timezone", bases := #[], mro := #[], ns := {}, slots := none }
        match cls with
        | .classObj cref => heapSetClassData cref {
            name := "Timezone", bases := #[], mro := #[cls], ns := {}, slots := none }
        | _ => pure ()
        let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
        return .instance instRef
      else if name == "datetime.datetime.now" then do
        -- Simplified: return a dummy datetime
        let ms ← (IO.monoMsNow : BaseIO Nat)
        let totalSec := ms / 1000
        let mut attrs : Std.HashMap String Value := {}
        attrs := attrs.insert "_year"        (.int 2024)
        attrs := attrs.insert "_month"       (.int 1)
        attrs := attrs.insert "_day"         (.int 1)
        attrs := attrs.insert "_hour"        (.int (totalSec / 3600 % 24))
        attrs := attrs.insert "_minute"      (.int (totalSec / 60 % 60))
        attrs := attrs.insert "_second"      (.int (totalSec % 60))
        attrs := attrs.insert "_microsecond" (.int 0)
        let cls ← allocClassObj {
          name := "Datetime", bases := #[], mro := #[], ns := {}, slots := none }
        match cls with
        | .classObj cref => heapSetClassData cref {
            name := "Datetime", bases := #[], mro := #[cls], ns := {}, slots := none }
        | _ => pure ()
        let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
        return .instance instRef
      else if name == "datetime.datetime.fromtimestamp" then do
        let ts := match args with
          | [.float f] => f
          | [.int n] => Float.ofInt n
          | _ => 0.0
        let totalSec := ts.toUInt64.toNat
        let mut attrs : Std.HashMap String Value := {}
        attrs := attrs.insert "_year"        (.int 1970)
        attrs := attrs.insert "_month"       (.int 1)
        attrs := attrs.insert "_day"         (.int (totalSec / 86400 + 1))
        attrs := attrs.insert "_hour"        (.int (totalSec / 3600 % 24))
        attrs := attrs.insert "_minute"      (.int (totalSec / 60 % 60))
        attrs := attrs.insert "_second"      (.int (totalSec % 60))
        attrs := attrs.insert "_microsecond" (.int 0)
        let cls ← allocClassObj {
          name := "Datetime", bases := #[], mro := #[], ns := {}, slots := none }
        match cls with
        | .classObj cref => heapSetClassData cref {
            name := "Datetime", bases := #[], mro := #[cls], ns := {}, slots := none }
        | _ => pure ()
        let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
        return .instance instRef
      -- ============================================================
      -- pathlib module constructor
      -- ============================================================
      else if name == "pathlib.Path" || name == "pathlib.PurePath" then do
        let path := match args with
          | [.str s] => s
          | [] => "."
          | _ =>
            -- Join multiple args
            let strs := args.filterMap fun v => match v with | .str s => some s | _ => none
            strs.foldl (fun acc s =>
              if acc.isEmpty then s
              else if s.startsWith "/" then s
              else acc ++ "/" ++ s) ""
        mkPathInstance path
      -- ============================================================
      -- logging module constructors
      -- ============================================================
      else if name == "logging.getLogger" then do
        let loggerName := match args with
          | [.str s] => s
          | [] => "root"
          | _ => "root"
        mkLoggerInstance loggerName 30  -- WARNING level by default
      else if name == "logging.Logger" then do
        let loggerName := match args with
          | [.str s] => s
          | _ => "root"
        mkLoggerInstance loggerName 30
      else if name == "logging.StreamHandler" || name == "logging.NullHandler" ||
              name == "logging.Formatter" then do
        -- Stub: return a no-op instance
        let clsName := if name == "logging.StreamHandler" then "StreamHandler"
          else if name == "logging.Formatter" then "Formatter"
          else "NullHandler"
        let mut attrs : Std.HashMap String Value := {}
        attrs := attrs.insert "_name" (.str clsName)
        let cls ← allocClassObj {
          name := clsName, bases := #[], mro := #[], ns := {}, slots := none }
        match cls with
        | .classObj cref => heapSetClassData cref {
            name := clsName, bases := #[], mro := #[cls], ns := {}, slots := none }
        | _ => pure ()
        let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
        return .instance instRef
      -- ============================================================
      -- threading module constructors
      -- ============================================================
      else if name == "threading.Lock" || name == "threading.RLock" then do
        let clsName := if name == "threading.Lock" then "Lock" else "RLock"
        let mut attrs : Std.HashMap String Value := {}
        attrs := attrs.insert "_locked" (.bool false)
        let cls ← allocClassObj {
          name := clsName, bases := #[], mro := #[], ns := {}, slots := none }
        match cls with
        | .classObj cref => heapSetClassData cref {
            name := clsName, bases := #[], mro := #[cls], ns := {}, slots := none }
        | _ => pure ()
        let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
        return .instance instRef
      else if name == "threading.Event" then do
        let mut attrs : Std.HashMap String Value := {}
        attrs := attrs.insert "_flag" (.bool false)
        let cls ← allocClassObj {
          name := "Event", bases := #[], mro := #[], ns := {}, slots := none }
        match cls with
        | .classObj cref => heapSetClassData cref {
            name := "Event", bases := #[], mro := #[cls], ns := {}, slots := none }
        | _ => pure ()
        let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
        return .instance instRef
      -- ============================================================
      -- tempfile module constructors
      -- ============================================================
      else if name == "tempfile.NamedTemporaryFile" then do
        let suffix ← (IO.rand 100000 999999 : IO Nat)
        let tmpPath := s!"/tmp/leanpy_{suffix}"
        let mut attrs : Std.HashMap String Value := {}
        attrs := attrs.insert "name" (.str tmpPath)
        attrs := attrs.insert "_buffer" (.bytes ByteArray.empty)
        let cls ← allocClassObj {
          name := "NamedTemporaryFile", bases := #[], mro := #[], ns := {}, slots := none }
        match cls with
        | .classObj cref => heapSetClassData cref {
            name := "NamedTemporaryFile", bases := #[], mro := #[cls], ns := {}, slots := none }
        | _ => pure ()
        let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
        return .instance instRef
      -- ============================================================
      -- sys.getrecursionlimit / sys.setrecursionlimit stubs
      -- ============================================================
      else if name == "sys.getrecursionlimit" then
        return .int 1000
      else if name == "sys.setrecursionlimit" then
        return .none
      else
        callBuiltin name args kwargs
  | .function ref => do
    let fd ← heapGetFunc ref
    callUserFunc fd args kwargs
  | .boundMethod receiver method => callBoundMethod receiver method args kwargs
  | .classObj cref => do
    let cd ← heapGetClassData cref
    -- Step 1: Look up __new__ in MRO
    let mut newFound : Option (Value × Value) := none
    for mroEntry in cd.mro do
      match mroEntry with
      | .classObj mref => do
        let mcd ← heapGetClassData mref
        if let some fn := mcd.ns["__new__"]? then
          newFound := some (fn, mroEntry)
          break
      | _ => pure ()
    -- Step 2: Create the instance via __new__ or default allocation
    let inst ← match newFound with
    | some (fn, definingCls) =>
      -- __new__ is implicitly static: unwrap staticMethod if present
      let rawFn := match fn with
        | .staticMethod inner => inner
        | other => other
      match rawFn with
      | .function fref => do
        let fd ← heapGetFunc fref
        let scope ← bindFuncParams fd.params (callee :: args) kwargs fd.defaults fd.kwDefaults
        let scopeWithClass := scope.insert "__class__" definingCls
        callRegularFunc fd scopeWithClass
      | _ => callValueDispatch rawFn (callee :: args) kwargs
    | none => allocInstance { cls := callee, attrs := {} }
    -- Step 3: Call __init__ only if inst is an instance of this class
    match inst with
    | .instance iref => do
      let instData ← heapGetInstanceData iref
      let isOurInstance := match instData.cls with
        | .classObj cr => cr == cref || (cd.mro.any fun e =>
            match e with | .classObj er => er == cr | _ => false)
        | _ => false
      if isOurInstance then
        -- Pydantic: apply model_validator(before) and field_validator(before)
        let isPydantic := cd.ns["__pydantic_model__"]? == some (.bool true)
        let (args', kwargs') ← if isPydantic then do
          -- model_validator(mode="before"): transform entire kwargs dict
          let mut kw := kwargs
          match cd.ns["__pydantic_model_validators_before__"]? with
          | some (.list mvRef) => do
            let mvArr ← heapGetList mvRef
            for validatorFn in mvArr do
              -- Build a dict from kwargs, call validator(cls, data)
              let kvPairs := kw.map (fun (k, v) => (Value.str k, v)) |>.toArray
              let kwDict ← allocDict kvPairs
              let result ← callValueDispatch validatorFn [callee, kwDict] []
              -- Extract updated kwargs from result dict
              match result with
              | .dict dref => do
                let pairs ← heapGetDict dref
                kw := pairs.toList.filterMap fun (k, v) =>
                  match k with | .str s => some (s, v) | _ => none
              | _ => pure ()  -- validator returned non-dict, keep current
          | _ => pure ()
          -- field_validator(mode="before"): transform individual field values
          match cd.ns["__pydantic_field_validators__"]? with
          | some (.list fvRef) => do
            let fvArr ← heapGetList fvRef
            for fv in fvArr do
              match fv with
              | .tuple elems =>
                if elems.size == 3 then
                  match elems[0]!, elems[1]! with
                  | .str fname, .str "before" =>
                    let validatorFn := elems[2]!
                    -- Find this field in kwargs
                    match kw.find? (fun (k, _) => k == fname) with
                    | some (_, fieldVal) =>
                      -- Call validator: it was a classmethod, so pass cls as first arg
                      let validatedVal ← callValueDispatch validatorFn [callee, fieldVal] []
                      kw := kw.map fun (k, v) =>
                        if k == fname then (k, validatedVal) else (k, v)
                    | none => pure ()
                  | _, _ => pure ()
              | _ => pure ()
          | _ => pure ()
          -- Resolve aliases: map aliased kwarg names back to Python field names
          let fieldNameStrs ← match cd.ns["__pydantic_field_names__"]? with
            | some (.list fnRef) => do
              let fnArr ← heapGetList fnRef
              pure (fnArr.toList.filterMap fun | .str s => some s | _ => none)
            | _ => pure ([] : List String)
          match cd.ns["__pydantic_alias_generator__"]? with
          | some agFn => do
            -- Build alias-to-field map
            let mut aliasToField : List (String × String) := []
            for fname in fieldNameStrs do
              let aliasVal ← callValueDispatch agFn [.str fname] []
              match aliasVal with
              | .str aliasName =>
                if aliasName != fname then
                  aliasToField := aliasToField ++ [(aliasName, fname)]
              | _ => pure ()
            -- Remap kwargs: alias → field name (only for keys not already a field name)
            let populateByName := cd.ns["__pydantic_populate_by_name__"]? == some (.bool true)
            let mut kw' : List (String × Value) := []
            for (kwName, kwVal) in kw do
              if fieldNameStrs.contains kwName then
                kw' := kw' ++ [(kwName, kwVal)]
              else
                match aliasToField.find? (fun (a, _) => a == kwName) with
                | some (_, fname) =>
                  if populateByName || !fieldNameStrs.contains kwName then
                    kw' := kw' ++ [(fname, kwVal)]
                  else
                    kw' := kw' ++ [(kwName, kwVal)]
                | none => kw' := kw' ++ [(kwName, kwVal)]
            kw := kw'
          | none => pure ()
          -- extra="forbid": reject unknown keyword arguments
          match cd.ns["__pydantic_extra__"]? with
          | some (.str "forbid") => do
            for (kwName, _) in kw do
              if !fieldNameStrs.contains kwName then
                throwValueError s!"Unexpected keyword argument '{kwName}' for {cd.name} (extra fields not permitted)"
          | _ => pure ()
          -- Apply __get_pydantic_core_schema__ field validation
          match cd.ns["__pydantic_field_schemas__"]? with
          | some (.dict fsRef) => do
            let fsPairs ← heapGetDict fsRef
            for (fnameV, schema) in fsPairs do
              match fnameV with
              | .str fname =>
                match kw.find? (fun (k, _) => k == fname) with
                | some (_, fieldVal) =>
                  let validated ← evalCoreSchema schema fieldVal
                  kw := kw.map fun (k, v) =>
                    if k == fname then (k, validated) else (k, v)
                | none => pure ()
              | _ => pure ()
          | _ => pure ()
          pure (args, kw)
        else
          pure (args, kwargs)
        let mut initFound : Option (Value × Value) := none
        for mroEntry in cd.mro do
          match mroEntry with
          | .classObj mref => do
            let mcd ← heapGetClassData mref
            if let some fn := mcd.ns["__init__"]? then
              initFound := some (fn, mroEntry)
              break
          | _ => pure ()
        match initFound with
        | some (fn, definingCls) =>
          match fn with
          | .function fref => do
            let fd ← heapGetFunc fref
            let scope ← bindFuncParams fd.params (inst :: args') kwargs' fd.defaults fd.kwDefaults
            let scopeWithClass := scope.insert "__class__" definingCls
            let _ ← callRegularFunc fd scopeWithClass
          | _ => let _ ← callValueDispatch fn (inst :: args') kwargs'
        | none =>
          if !args'.isEmpty && newFound.isNone then
            throwTypeError s!"{cd.name}() takes no arguments"
        -- Pydantic: apply model_validator(mode="after")
        if isPydantic then
          match cd.ns["__pydantic_model_validators_after__"]? with
          | some (.list mvRef) => do
            let mvArr ← heapGetList mvRef
            for validatorFn in mvArr do
              -- Call validator with self; it returns self (or raises)
              let _ ← callValueDispatch validatorFn [inst] []
          | _ => pure ()
      return inst
    | other => return other  -- __new__ returned non-instance; skip __init__
  | .instance _ => do
    -- Try __call__ dunder
    match ← callDunder callee "__call__" args with
    | some v => return v
    | none => throwTypeError s!"'{typeName callee}' object is not callable"
  | .exception _ _ =>
    -- Exception objects are not callable, but exception classes are handled as builtins
    throwTypeError s!"'{typeName callee}' object is not callable"
  | _ => throwTypeError s!"'{typeName callee}' object is not callable"

-- Call a user-defined function
partial def callUserFunc (fd : FuncData) (args : List Value)
    (kwargs : List (String × Value)) : InterpM Value := do
  let scope ← bindFuncParams fd.params args kwargs fd.defaults fd.kwDefaults
  if fd.isGenerator then
    callGeneratorFunc fd scope
  else
    callRegularFunc fd scope

-- Call a regular (non-generator) function
partial def callRegularFunc (fd : FuncData) (scope : Scope) : InterpM Value := do
  let st ← get
  let savedLocal := st.localScopes
  let savedGlobalDecls := st.globalDecls
  let savedNonlocalDecls := st.nonlocalDecls
  set { st with
    localScopes   := [scope] ++ fd.closure.toList
    globalDecls   := [{}]
    nonlocalDecls := [{}] }
  let result ← do
    try
      execStmts fd.body
      pure Value.none
    catch
    | .control (.return_ v) => pure v
    | other => throw other
  modify fun st' => { st' with
    localScopes   := savedLocal
    globalDecls   := savedGlobalDecls
    nonlocalDecls := savedNonlocalDecls }
  return result

-- Call a generator function: execute body eagerly, collecting yielded values
partial def callGeneratorFunc (fd : FuncData) (scope : Scope) : InterpM Value := do
  let st ← get
  let savedLocal := st.localScopes
  let savedGlobalDecls := st.globalDecls
  let savedNonlocalDecls := st.nonlocalDecls
  let savedAccumulator := st.yieldAccumulator
  set { st with
    localScopes    := [scope] ++ fd.closure.toList
    globalDecls    := [{}]
    nonlocalDecls  := [{}]
    yieldAccumulator := some #[] }
  let _ ← do
    try
      execStmts fd.body
      pure Value.none
    catch
    | .control (.return_ _) => pure Value.none
    | other => throw other
  let values := match (← get).yieldAccumulator with
    | some acc => acc
    | none => #[]
  modify fun st' => { st' with
    localScopes    := savedLocal
    globalDecls    := savedGlobalDecls
    nonlocalDecls  := savedNonlocalDecls
    yieldAccumulator := savedAccumulator }
  allocGenerator values

-- Bind function arguments to parameters
partial def bindFuncParams (params : Arguments) (args : List Value)
    (kwargs : List (String × Value)) (defaults : Array Value)
    (kwDefaults : Array (Option Value)) : InterpM Scope := do
  let mut scope : Scope := {}
  let allPosParams := params.posOnlyArgs ++ params.args
  let numPos := allPosParams.length
  let numDefaults := defaults.size
  -- Bind positional arguments
  let mut posIdx : Nat := 0
  for i in [:min args.length numPos] do
    scope := scope.insert allPosParams[i]!.name (args[i]!)
    posIdx := i + 1
  -- Extra positional args go to *args
  if let some va := params.varArg then
    let extra := args.drop numPos
    let ref ← heapAlloc (.listObj extra.toArray)
    scope := scope.insert va.name (.list ref)
  else if args.length > numPos then
    throwTypeError "too many positional arguments"
  -- Fill defaults for unfilled positional params
  for i in [posIdx:numPos] do
    if scope.contains allPosParams[i]!.name then continue
    -- Check kwargs first
    match kwargs.find? fun (k, _) => k == allPosParams[i]!.name with
    | some (_, v) => scope := scope.insert allPosParams[i]!.name v
    | none =>
      let defaultIdx : Int := (i : Int) + (numDefaults : Int) - (numPos : Int)
      if defaultIdx >= 0 && defaultIdx.toNat < numDefaults then
        scope := scope.insert allPosParams[i]!.name defaults[defaultIdx.toNat]!
      else
        throwTypeError s!"missing required argument: '{allPosParams[i]!.name}'"
  -- Bind remaining keyword arguments
  for (name, value) in kwargs do
    let isPosParam := allPosParams.any fun a => a.name == name
    if isPosParam then
      if scope.contains name then pure () -- already bound by positional
      else scope := scope.insert name value
    else
      scope := scope.insert name value
  -- Fill keyword-only defaults
  for i in [:params.kwOnlyArgs.length] do
    let kwParam := params.kwOnlyArgs[i]!
    if !scope.contains kwParam.name then
      if i < kwDefaults.size then
        match kwDefaults[i]! with
        | some v => scope := scope.insert kwParam.name v
        | none => throwTypeError s!"missing keyword argument: '{kwParam.name}'"
      else
        throwTypeError s!"missing keyword argument: '{kwParam.name}'"
  -- **kwargs parameter
  if let some kwa := params.kwArg then
    let mut extraKw : Array (Value × Value) := #[]
    for (name, value) in kwargs do
      let isKnown := allPosParams.any (fun a => a.name == name)
                    || params.kwOnlyArgs.any (fun a => a.name == name)
      if !isKnown then
        extraKw := extraKw.push (.str name, value)
    let ref ← heapAlloc (.dictObj extraKw)
    scope := scope.insert kwa.name (.dict ref)
  return scope

-- Comprehension evaluation
partial def evalCompGen (generators : List Comprehension)
    (body : InterpM Value) (acc : Array Value) : InterpM (Array Value) := do
  match generators with
  | [] => return acc.push (← body)
  | gen :: rest => do
    let items ← iterValues (← evalExpr gen.iter)
    let mut result := acc
    for item in items do
      assignToTarget gen.target item
      let mut pass := true
      for cond in gen.ifs do
        if !(← isTruthy (← evalExpr cond)) then pass := false; break
      if pass then result ← evalCompGen rest body result
    return result

partial def evalDictCompGen (generators : List Comprehension)
    (body : InterpM (Value × Value)) (acc : Array (Value × Value))
    : InterpM (Array (Value × Value)) := do
  match generators with
  | [] => return acc.push (← body)
  | gen :: rest => do
    let items ← iterValues (← evalExpr gen.iter)
    let mut result := acc
    for item in items do
      assignToTarget gen.target item
      let mut pass := true
      for cond in gen.ifs do
        if !(← isTruthy (← evalExpr cond)) then pass := false; break
      if pass then result ← evalDictCompGen rest body result
    return result

-- Attribute access
partial def getAttributeValue (obj : Value) (attr : String) : InterpM Value := do
  match obj with
  | .str _ =>
    if knownStrMethods.contains attr then return .boundMethod obj attr
    else throwAttributeError s!"'str' object has no attribute '{attr}'"
  | .list _ =>
    if knownListMethods.contains attr then return .boundMethod obj attr
    else throwAttributeError s!"'list' object has no attribute '{attr}'"
  | .dict ref =>
    if knownDictMethods.contains attr then return .boundMethod obj attr
    else do
      -- Check dict entries as attributes (for class/object dicts)
      let pairs ← heapGetDict ref
      for (k, v) in pairs do
        match k with
        | .str s => if s == attr then return v
        | _ => pure ()
      throwAttributeError s!"'dict' object has no attribute '{attr}'"
  | .set _ =>
    if knownSetMethods.contains attr then return .boundMethod obj attr
    else throwAttributeError s!"'set' object has no attribute '{attr}'"
  | .int _ =>
    if knownIntMethods.contains attr then return .boundMethod obj attr
    else throwAttributeError s!"'int' object has no attribute '{attr}'"
  | .bytes _ =>
    if knownBytesMethods.contains attr then return .boundMethod obj attr
    else throwAttributeError s!"'bytes' object has no attribute '{attr}'"
  | .tuple _ =>
    if knownTupleMethods.contains attr then return .boundMethod obj attr
    else throwAttributeError s!"'tuple' object has no attribute '{attr}'"
  | .builtin name =>
    -- Type methods like int.from_bytes, bytes.fromhex, object.__new__
    match name, attr with
    | "int", "from_bytes" => return .boundMethod obj attr
    | "bytes", "fromhex" => return .boundMethod obj attr
    | "object", "__new__" => return .boundMethod obj attr
    | "object", "__setattr__" => return .boundMethod obj attr
    | "object", "__delattr__" => return .boundMethod obj attr
    -- datetime.timezone.utc — construct a UTC timezone instance
    | "datetime.timezone", "utc" => do
      let mut attrs : Std.HashMap String Value := {}
      attrs := attrs.insert "_name" (.str "UTC")
      attrs := attrs.insert "_offset" (.int 0)
      let cls ← allocClassObj {
        name := "Timezone", bases := #[], mro := #[], ns := {}, slots := none }
      match cls with
      | .classObj cref => heapSetClassData cref {
          name := "Timezone", bases := #[], mro := #[cls], ns := {}, slots := none }
      | _ => pure ()
      let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
      return .instance instRef
    -- datetime.datetime.now — class method
    | "datetime.datetime", "now" => return .builtin "datetime.datetime.now"
    | "datetime.datetime", "fromtimestamp" => return .builtin "datetime.datetime.fromtimestamp"
    | _, _ => throwAttributeError s!"type '{name}' has no attribute '{attr}'"
  | .exception _ _ =>
    match attr with
    | "args" => return .str (Value.toStr obj)
    | _ => throwAttributeError s!"'{typeName obj}' object has no attribute '{attr}'"
  | .generator _ =>
    if attr == "__next__" || attr == "__iter__" || attr == "close" then
      return .boundMethod obj attr
    else throwAttributeError s!"'generator' object has no attribute '{attr}'"
  | .module mref => do
    let md ← heapGetModuleData mref
    if attr == "__name__" then return .str md.name
    if attr == "__file__" then
      match md.file with
      | some f => return .str f
      | none => return .none
    if attr == "__package__" then
      match md.package with
      | some p => return .str p
      | none => return .none
    match md.ns[attr]? with
    | some v => return v
    | none => throwAttributeError s!"module '{md.name}' has no attribute '{attr}'"
  | .classObj cref => do
    let cd ← heapGetClassData cref
    -- Special attributes on class objects
    if attr == "__name__" then return .str cd.name
    if attr == "__mro__" then return .tuple cd.mro
    if attr == "__dict__" then
      let mut pairs : Array (Value × Value) := #[]
      for (k, v) in cd.ns do
        pairs := pairs.push (.str k, v)
      return ← allocDict pairs
    -- Look up in own namespace first, then walk MRO
    for mroEntry in cd.mro do
      match mroEntry with
      | .classObj mref => do
        let mcd ← heapGetClassData mref
        if let some v := mcd.ns[attr]? then
          match v with
          | .staticMethod fn => return fn
          | .classMethod _ => return .boundMethod obj attr
          | .property _ _ _ => return v  -- on class, return the descriptor itself
          | _ => return v
      | _ => pure ()
    throwAttributeError s!"type object '{cd.name}' has no attribute '{attr}'"
  | .instance iref => do
    let id_ ← heapGetInstanceData iref
    -- Special attributes: __dict__ and __class__
    if attr == "__dict__" then
      let mut pairs : Array (Value × Value) := #[]
      for (k, v) in id_.attrs do
        pairs := pairs.push (.str k, v)
      return ← allocDict pairs
    if attr == "__class__" then return id_.cls
    -- Check for data descriptors (property) in MRO first (before instance attrs)
    match id_.cls with
    | .classObj cref => do
      let cd ← heapGetClassData cref
      -- Builtin instance types: return bound methods for known method names
      if cd.name == "BytesIO" && ["write", "read", "getvalue", "seek", "tell",
            "__enter__", "__exit__", "close"].contains attr then
        match obj with
        | .instance _ => return .boundMethod obj attr
        | _ => pure ()
      if cd.name == "StringIO" && ["write", "read", "getvalue", "seek", "tell",
            "__enter__", "__exit__", "close"].contains attr then
        match obj with
        | .instance _ => return .boundMethod obj attr
        | _ => pure ()
      if (cd.name == "SHA256Hash" || cd.name == "SHAKE128Hash") &&
            ["update", "digest", "hexdigest", "copy"].contains attr then
        match obj with
        | .instance _ => return .boundMethod obj attr
        | _ => pure ()
      if cd.name == "HMAC" && ["update", "digest", "hexdigest", "copy"].contains attr then
        match obj with
        | .instance _ => return .boundMethod obj attr
        | _ => pure ()
      if cd.name == "TextIOWrapper" && ["write", "flush", "fileno", "isatty",
            "readable", "writable", "seekable", "close",
            "__enter__", "__exit__"].contains attr then
        match obj with
        | .instance _ => return .boundMethod obj attr
        | _ => pure ()
      if cd.name == "Datetime" then
        -- Properties: return instance attrs directly
        if ["year", "month", "day", "hour", "minute", "second"].contains attr then
          match id_.attrs[s!"_{attr}"]? with
          | some v => return v
          | none => pure ()
        -- Methods
        if ["isoformat", "timestamp", "replace",
              "__str__", "__repr__"].contains attr then
          match obj with
          | .instance _ => return .boundMethod obj attr
          | _ => pure ()
      if cd.name == "Timedelta" then
        -- Properties
        if ["days", "seconds", "microseconds"].contains attr then
          match id_.attrs[attr]? with
          | some v => return v
          | none => pure ()
        -- Methods
        if ["total_seconds", "__str__", "__repr__"].contains attr then
          match obj with
          | .instance _ => return .boundMethod obj attr
          | _ => pure ()
      if cd.name == "Timezone" && ["__str__", "__repr__",
            "tzname", "utcoffset"].contains attr then
        match obj with
        | .instance _ => return .boundMethod obj attr
        | _ => pure ()
      if cd.name == "PurePath" then
        -- Properties: call immediately and return the value
        if ["name", "stem", "suffix", "parts"].contains attr then
          match obj with
          | .instance iref_ => return ← callPathMethod iref_ attr []
          | _ => pure ()
        -- parent returns a new PurePath instance
        if attr == "parent" then
          match obj with
          | .instance iref_ => return ← callPathMethod iref_ "parent" []
          | _ => pure ()
        -- Methods: return as bound methods
        if ["exists", "is_file", "is_dir", "resolve",
            "read_text", "read_bytes", "__truediv__", "__str__",
            "__repr__", "__fspath__", "with_suffix"].contains attr then
          match obj with
          | .instance _ => return .boundMethod obj attr
          | _ => pure ()
      if cd.name == "Logger" && ["debug", "info", "warning", "warn",
            "error", "critical", "setLevel", "getChild", "addHandler",
            "isEnabledFor", "getEffectiveLevel", "__repr__"].contains attr then
        match obj with
        | .instance _ => return .boundMethod obj attr
        | _ => pure ()
      if (cd.name == "StreamHandler" || cd.name == "NullHandler" ||
            cd.name == "Formatter") &&
            ["setLevel", "setFormatter", "format", "emit", "close"].contains attr then
        match obj with
        | .instance _ => return .boundMethod obj attr
        | _ => pure ()
      if (cd.name == "Lock" || cd.name == "RLock") && ["acquire", "release",
            "locked", "__enter__", "__exit__"].contains attr then
        match obj with
        | .instance _ => return .boundMethod obj attr
        | _ => pure ()
      if cd.name == "Event" && ["set", "clear", "is_set", "wait"].contains attr then
        match obj with
        | .instance _ => return .boundMethod obj attr
        | _ => pure ()
      if cd.name == "NamedTemporaryFile" && ["write", "read", "close",
            "__enter__", "__exit__"].contains attr then
        match obj with
        | .instance _ => return .boundMethod obj attr
        | _ => pure ()
      -- First pass: check for data descriptors (property with getter)
      for mroEntry in cd.mro do
        match mroEntry with
        | .classObj mref => do
          let mcd ← heapGetClassData mref
          if let some v := mcd.ns[attr]? then
            match v with
            | .property getter _ _ =>
              -- Call the getter with self
              match getter with
              | .function fref => do
                let fd ← heapGetFunc fref
                let scope ← bindFuncParams fd.params [obj] [] fd.defaults fd.kwDefaults
                return ← callRegularFunc fd scope
              | .none => throwAttributeError s!"unreadable attribute '{attr}'"
              | _ => return ← callValueDispatch getter [obj] []
            | _ => pure ()  -- not a data descriptor, continue
        | _ => pure ()
      -- Pydantic model methods: return as bound methods
      if cd.ns["__pydantic_model__"]? == some (.bool true) &&
            ["model_copy", "model_dump"].contains attr then
        return .boundMethod obj attr
      -- Check instance attributes
      if let some v := id_.attrs[attr]? then return v
      -- Then walk class MRO for non-data descriptors
      for mroEntry in cd.mro do
        match mroEntry with
        | .classObj mref => do
          let mcd ← heapGetClassData mref
          if let some v := mcd.ns[attr]? then
            match v with
            | .function _ => return .boundMethod obj attr
            | .staticMethod fn => return fn
            | .classMethod _ => return .boundMethod id_.cls attr
            | _ => return v
        | _ => pure ()
      -- Fallback: try __getattr__ hook before raising
      for mroEntry in cd.mro do
        match mroEntry with
        | .classObj mref => do
          let mcd ← heapGetClassData mref
          if let some fn := mcd.ns["__getattr__"]? then
            match fn with
            | .function fref => do
              let fd ← heapGetFunc fref
              let scope ← bindFuncParams fd.params [obj, .str attr] [] fd.defaults fd.kwDefaults
              let scopeWithClass := scope.insert "__class__" mroEntry
              return ← callRegularFunc fd scopeWithClass
            | _ => return ← callValueDispatch fn [obj, .str attr] []
        | _ => pure ()
      throwAttributeError s!"'{cd.name}' object has no attribute '{attr}'"
    | _ =>
      -- No class, check instance attrs only
      if let some v := id_.attrs[attr]? then return v
      throwAttributeError s!"instance has no attribute '{attr}'"
  | .property _ _ _ =>
    -- Attribute access on property object (e.g., prop.setter, prop.getter, prop.deleter)
    match attr with
    | "setter" | "getter" | "deleter" | "fget" | "fset" | "fdel" =>
      return .boundMethod obj attr
    | _ => throwAttributeError s!"'property' object has no attribute '{attr}'"
  | .superObj _startAfterCls _inst =>
    -- super().attr — resolved in callBoundMethod
    return .boundMethod obj attr
  | _ => throwAttributeError s!"'{typeName obj}' object has no attribute '{attr}'"

-- Subscript access
partial def evalSubscriptValue (obj idx : Value) : InterpM Value := do
  match obj with
  | .list ref => do
    let arr ← heapGetList ref
    match idx with
    | .int i =>
      let ni := normalizeIndex i arr.size
      if ni < 0 || ni >= arr.size then throwIndexError "list index out of range"
      return arr[ni.toNat]!
    | .tuple #[start, stop, step] => do
      let (s, e, st) ← computeSliceIndices arr.size (some start) (some stop) (some step)
      let result := (sliceIndices s e st).filterMap fun i => arr[i]?
      allocList result.toArray
    | _ => throwTypeError "list indices must be integers or slices"
  | .tuple arr =>
    match idx with
    | .int i =>
      let ni := normalizeIndex i arr.size
      if ni < 0 || ni >= arr.size then throwIndexError "tuple index out of range"
      return arr[ni.toNat]!
    | _ => throwTypeError "tuple indices must be integers"
  | .str s =>
    match idx with
    | .int i =>
      let chars := s.toList
      let ni := normalizeIndex i chars.length
      if ni < 0 || ni >= chars.length then throwIndexError "string index out of range"
      return .str (String.ofList [chars[ni.toNat]!])
    | .tuple #[start, stop, step] => do
      let chars := s.toList
      let (st, en, stp) ← computeSliceIndices chars.length (some start) (some stop) (some step)
      let result := (sliceIndices st en stp).filterMap fun i => chars[i]?
      return .str (String.ofList result)
    | _ => throwTypeError "string indices must be integers"
  | .dict ref => do
    let pairs ← heapGetDict ref
    for (k, v) in pairs do
      if ← valueEq k idx then return v
    -- Check if this dict has a default factory (collections.defaultdict)
    let st ← get
    if let some factory := st.defaultFactories[ref]? then
      if !Value.beq factory .none then
        let defaultVal ← callValueDispatch factory [] []
        heapSetDict ref (pairs.push (idx, defaultVal))
        return defaultVal
    throwKeyError (← valueRepr idx)
  | .bytes b =>
    match idx with
    | .int i =>
      let ni := normalizeIndex i b.size
      if ni < 0 || ni >= b.size then throwIndexError "index out of range"
      return .int b[ni.toNat]!.toNat
    | _ => throwTypeError "bytes indices must be integers"
  | .instance _ => do
    match ← callDunder obj "__getitem__" [idx] with
    | some v => return v
    | none => throwTypeError s!"'{typeName obj}' object is not subscriptable"
  | _ => throwTypeError s!"'{typeName obj}' object is not subscriptable"

-- Assignment target resolution
partial def assignToTarget (target : Expr) (value : Value) : InterpM Unit := do
  match target with
  | .name n _ => setVariable n value
  | .subscript obj idx _ => do
    let objVal ← evalExpr obj
    let idxVal ← evalExpr idx
    assignSubscriptValue objVal idxVal value
  | .tuple targets _ | .list_ targets _ => do
    let items ← iterValues value
    -- Check for starred
    let starIdx := targets.findIdx? fun
      | .starred _ _ => true
      | _ => false
    match starIdx with
    | none =>
      if items.size != targets.length then
        throwValueError s!"expected {targets.length} values to unpack, got {items.size}"
      for i in [:targets.length] do
        assignToTarget targets[i]! items[i]!
    | some si => do
      let before := targets.take si
      let after := targets.drop (si + 1)
      if items.size < before.length + after.length then
        throwValueError "not enough values to unpack"
      for i in [:before.length] do
        assignToTarget before[i]! items[i]!
      let starCount := items.size - before.length - after.length
      match targets[si]! with
      | .starred inner _ => do
        let starItems := items.toList.drop before.length |>.take starCount
        let starList ← allocList starItems.toArray
        assignToTarget inner starList
      | _ => pure ()
      for i in [:after.length] do
        assignToTarget after[i]! items[items.size - after.length + i]!
  | .attribute obj attr _ => do
    let objVal ← evalExpr obj
    match objVal with
    | .module mref => do
      let md ← heapGetModuleData mref
      heapSetModuleData mref { md with ns := md.ns.insert attr value }
    | .dict ref => do
      let pairs ← heapGetDict ref
      let key := Value.str attr
      let mut newPairs := pairs
      let mut found := false
      for i in [:pairs.size] do
        if pairs[i]!.1 == key then
          newPairs := newPairs.set! i (key, value)
          found := true
          break
      if !found then newPairs := newPairs.push (key, value)
      heapSetDict ref newPairs
    | .instance iref => do
      let id_ ← heapGetInstanceData iref
      -- Check for __setattr__ hook first
      let mut setattrFound : Option (Value × Value) := none
      match id_.cls with
      | .classObj cref => do
        let cd ← heapGetClassData cref
        for mroEntry in cd.mro do
          if setattrFound.isSome then break
          match mroEntry with
          | .classObj mref => do
            let mcd ← heapGetClassData mref
            if let some fn := mcd.ns["__setattr__"]? then
              setattrFound := some (fn, mroEntry)
          | _ => pure ()
      | _ => pure ()
      match setattrFound with
      | some (fn, definingCls) =>
        match fn with
        | .function fref => do
          let fd ← heapGetFunc fref
          let scope ← bindFuncParams fd.params [objVal, .str attr, value] [] fd.defaults fd.kwDefaults
          let scopeWithClass := scope.insert "__class__" definingCls
          let _ ← callRegularFunc fd scopeWithClass
          return ()
        | _ =>
          let _ ← callValueDispatch fn [objVal, .str attr, value] []
          return ()
      | none => pure ()  -- fall through to property / direct store
      -- Check if the class has a property setter for this attr
      let mut propSetter : Option Value := none
      match id_.cls with
      | .classObj cref => do
        let cd ← heapGetClassData cref
        for mroEntry in cd.mro do
          if propSetter.isSome then break
          match mroEntry with
          | .classObj mref => do
            let mcd ← heapGetClassData mref
            match mcd.ns[attr]? with
            | some (.property _ (some setter) _) => propSetter := some setter
            | some (.property _ none _) => pure ()  -- property without setter
            | _ => pure ()
          | _ => pure ()
      | _ => pure ()
      match propSetter with
      | some setter => do
        let _ ← callValueDispatch setter [objVal, value] []
        pure ()
      | none => do
        -- Check __slots__ restriction
        let id2 ← heapGetInstanceData iref
        match id2.cls with
        | .classObj cref2 => do
          let cd2 ← heapGetClassData cref2
          if let some allowedSlots := cd2.slots then
            if !allowedSlots.contains attr then
              throwAttributeError s!"'{cd2.name}' object has no attribute '{attr}'"
        | _ => pure ()
        heapSetInstanceData iref { id_ with attrs := id_.attrs.insert attr value }
    | .classObj cref => do
      let cd ← heapGetClassData cref
      heapSetClassData cref { cd with ns := cd.ns.insert attr value }
    | _ => throwAttributeError s!"'{typeName objVal}' object attribute '{attr}' is read-only"
  | .starred inner _ => assignToTarget inner value
  | _ => throwRuntimeError (.runtimeError "invalid assignment target")

partial def assignSubscriptValue (obj idx value : Value) : InterpM Unit := do
  match obj with
  | .list ref =>
    match idx with
    | .int i => do
      let arr ← heapGetList ref
      let ni := normalizeIndex i arr.size
      if ni < 0 || ni >= arr.size then throwIndexError "list assignment index out of range"
      heapSetList ref (arr.set! ni.toNat value)
    | _ => throwTypeError "list indices must be integers"
  | .dict ref => do
    let pairs ← heapGetDict ref
    let mut newPairs := pairs
    let mut found := false
    for i in [:pairs.size] do
      if ← valueEq pairs[i]!.1 idx then
        newPairs := newPairs.set! i (idx, value)
        found := true
        break
    if !found then newPairs := newPairs.push (idx, value)
    heapSetDict ref newPairs
  | .instance _ => do
    match ← callDunder obj "__setitem__" [idx, value] with
    | some _ => return ()
    | none => throwTypeError s!"'{typeName obj}' does not support item assignment"
  | _ => throwTypeError s!"'{typeName obj}' does not support item assignment"

-- ============================================================
-- Module system (inside mutual block since loadModule calls execStmts)
-- ============================================================

/-- Create a synthetic built-in module. Returns none if not a known builtin. -/
partial def getBuiltinModule (name : String) : InterpM (Option Value) := do
  let st ← get
  -- Check cache first
  if let some v := st.loadedModules[name]? then return some v
  let mkMod (ns : Std.HashMap String Value) : InterpM Value := do
    let md : ModuleData := {
      name := name, file := none, package := none, ns := ns, allNames := none }
    let modVal ← allocModule md
    modify fun st' => { st' with loadedModules := st'.loadedModules.insert name modVal }
    return modVal
  match name with
  | "__future__" =>
    let ns : Std.HashMap String Value := {}
    some <$> mkMod (ns.insert "annotations" .none)
  | "typing" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "TYPE_CHECKING" (.bool false)
    ns := ns.insert "get_type_hints" (.builtin "typing.get_type_hints")
    -- Stub common typing names as none so imports don't fail
    for n in ["Any", "Optional", "List", "Dict", "Set", "Tuple", "FrozenSet",
              "ClassVar", "Final", "Self", "Union", "Callable", "Protocol",
              "runtime_checkable", "NamedTuple",
              "override", "NoReturn", "SupportsInt", "SupportsIndex",
              "TypeAlias", "Literal", "IO", "Sequence", "Mapping",
              "Iterator", "Iterable", "Generator", "Coroutine",
              "Awaitable", "AsyncIterator", "AsyncGenerator",
              "Type", "Generic", "TypeVar", "Annotated",
              "overload", "cast", "no_type_check"] do
      ns := ns.insert n .none
    some <$> mkMod ns
  | "typing_extensions" =>
    -- Alias for typing for compatibility
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "TYPE_CHECKING" (.bool false)
    for n in ["override", "Self", "Protocol", "runtime_checkable",
              "Annotated", "TypeAlias", "get_type_hints"] do
      ns := ns.insert n .none
    some <$> mkMod ns
  | "abc" => do
    -- Create real ABC base class
    let abcCls ← allocClassObj {
      name := "ABC", bases := #[], mro := #[], ns := {}, slots := none }
    match abcCls with
    | .classObj ref => heapSetClassData ref {
        name := "ABC", bases := #[], mro := #[abcCls], ns := {}, slots := none }
    | _ => pure ()
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "ABC" abcCls
    ns := ns.insert "abstractmethod" (.builtin "abc.abstractmethod")
    ns := ns.insert "ABCMeta" .none
    some <$> mkMod ns
  | "dataclasses" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "dataclass" (.builtin "dataclass")
    ns := ns.insert "field" (.builtin "dataclasses.field")
    ns := ns.insert "fields" (.builtin "dataclasses.fields")
    some <$> mkMod ns
  | "functools" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "singledispatch" (.builtin "functools.singledispatch")
    ns := ns.insert "lru_cache"      (.builtin "functools.lru_cache")
    ns := ns.insert "reduce"         (.builtin "functools.reduce")
    ns := ns.insert "wraps"          (.builtin "functools.wraps")
    ns := ns.insert "partial"        (.builtin "functools.partial")
    ns := ns.insert "cached_property" (.builtin "functools.cached_property")
    ns := ns.insert "total_ordering" (.builtin "functools.total_ordering")
    some <$> mkMod ns
  | "enum" => do
    -- Create real Enum class
    let enumCls ← allocClassObj {
      name := "Enum", bases := #[], mro := #[], ns := {}, slots := none }
    -- Create IntEnum class (subclass of Enum)
    let intEnumCls ← allocClassObj {
      name := "IntEnum", bases := #[enumCls], mro := #[enumCls], ns := {}, slots := none }
    -- Fix MRO to include self
    match enumCls with
    | .classObj ref => heapSetClassData ref {
        name := "Enum", bases := #[], mro := #[enumCls], ns := {}, slots := none }
    | _ => pure ()
    match intEnumCls with
    | .classObj ref => heapSetClassData ref {
        name := "IntEnum", bases := #[enumCls], mro := #[intEnumCls, enumCls], ns := {}, slots := none }
    | _ => pure ()
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "Enum" enumCls
    ns := ns.insert "IntEnum" intEnumCls
    ns := ns.insert "auto" (.builtin "enum.auto")
    some <$> mkMod ns
  | "math" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "ceil"  (.builtin "math.ceil")
    ns := ns.insert "floor" (.builtin "math.floor")
    ns := ns.insert "sqrt"  (.builtin "math.sqrt")
    ns := ns.insert "log"   (.builtin "math.log")
    ns := ns.insert "log2"  (.builtin "math.log2")
    ns := ns.insert "fabs"  (.builtin "math.fabs")
    ns := ns.insert "isnan" (.builtin "math.isnan")
    ns := ns.insert "isinf" (.builtin "math.isinf")
    ns := ns.insert "inf"   (.float (1.0 / 0.0))
    ns := ns.insert "nan"   (.float (0.0 / 0.0))
    ns := ns.insert "pi"    (.float 3.141592653589793)
    ns := ns.insert "e"     (.float 2.718281828459045)
    some <$> mkMod ns
  | "copy" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "copy"     (.builtin "copy.copy")
    ns := ns.insert "deepcopy" (.builtin "copy.deepcopy")
    some <$> mkMod ns
  | "operator" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "itemgetter" (.builtin "operator.itemgetter")
    ns := ns.insert "attrgetter" (.builtin "operator.attrgetter")
    ns := ns.insert "add" (.builtin "operator.add")
    ns := ns.insert "sub" (.builtin "operator.sub")
    ns := ns.insert "mul" (.builtin "operator.mul")
    ns := ns.insert "eq"  (.builtin "operator.eq")
    ns := ns.insert "ne"  (.builtin "operator.ne")
    ns := ns.insert "lt"  (.builtin "operator.lt")
    ns := ns.insert "le"  (.builtin "operator.le")
    ns := ns.insert "gt"  (.builtin "operator.gt")
    ns := ns.insert "ge"  (.builtin "operator.ge")
    ns := ns.insert "neg" (.builtin "operator.neg")
    ns := ns.insert "not_" (.builtin "operator.not_")
    some <$> mkMod ns
  | "itertools" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "chain"      (.builtin "itertools.chain")
    ns := ns.insert "accumulate" (.builtin "itertools.accumulate")
    ns := ns.insert "count"      (.builtin "itertools.count")
    some <$> mkMod ns
  | "collections" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "defaultdict" (.builtin "collections.defaultdict")
    ns := ns.insert "OrderedDict" (.builtin "collections.OrderedDict")
    ns := ns.insert "deque"       (.builtin "collections.deque")
    some <$> mkMod ns
  | "collections.abc" =>
    let mut ns : Std.HashMap String Value := {}
    for n in ["Iterable", "Iterator", "Mapping", "Sequence", "Set",
              "MutableMapping", "MutableSequence", "MutableSet",
              "Callable", "Hashable", "Sized", "Container"] do
      ns := ns.insert n .none
    some <$> mkMod ns
  | "struct" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "pack"     (.builtin "struct.pack")
    ns := ns.insert "unpack"   (.builtin "struct.unpack")
    ns := ns.insert "calcsize" (.builtin "struct.calcsize")
    some <$> mkMod ns
  | "io" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "BytesIO"  (.builtin "io.BytesIO")
    ns := ns.insert "StringIO" (.builtin "io.StringIO")
    some <$> mkMod ns
  | "bisect" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "bisect_left"  (.builtin "bisect.bisect_left")
    ns := ns.insert "bisect_right" (.builtin "bisect.bisect_right")
    ns := ns.insert "bisect"       (.builtin "bisect.bisect")
    ns := ns.insert "insort"       (.builtin "bisect.insort")
    ns := ns.insert "insort_right" (.builtin "bisect.insort_right")
    ns := ns.insert "insort_left"  (.builtin "bisect.insort_left")
    some <$> mkMod ns
  | "base64" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "b64encode"         (.builtin "base64.b64encode")
    ns := ns.insert "b64decode"         (.builtin "base64.b64decode")
    ns := ns.insert "urlsafe_b64encode" (.builtin "base64.urlsafe_b64encode")
    ns := ns.insert "urlsafe_b64decode" (.builtin "base64.urlsafe_b64decode")
    ns := ns.insert "b16encode"         (.builtin "base64.b16encode")
    ns := ns.insert "b16decode"         (.builtin "base64.b16decode")
    some <$> mkMod ns
  | "json" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "dumps" (.builtin "json.dumps")
    ns := ns.insert "loads" (.builtin "json.loads")
    some <$> mkMod ns
  | "re" =>
    let mut ns : Std.HashMap String Value := {}
    for n in ["compile", "match", "search", "sub", "findall", "split",
              "IGNORECASE", "MULTILINE", "DOTALL", "VERBOSE"] do
      ns := ns.insert n (.builtin s!"re.{n}")
    some <$> mkMod ns
  | "hashlib" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "sha256"    (.builtin "hashlib.sha256")
    ns := ns.insert "shake_128" (.builtin "hashlib.shake_128")
    ns := ns.insert "new"       (.builtin "hashlib.new")
    some <$> mkMod ns
  | "hmac" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "new"    (.builtin "hmac.new")
    ns := ns.insert "digest" (.builtin "hmac.digest")
    some <$> mkMod ns
  | "secrets" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "token_bytes" (.builtin "secrets.token_bytes")
    ns := ns.insert "randbelow"   (.builtin "secrets.randbelow")
    some <$> mkMod ns
  | "sys" => do
    let st ← get
    let mut ns : Std.HashMap String Value := {}
    -- sys.path from searchPaths
    let pathArr := st.searchPaths.map (fun s => Value.str s)
    let pathRef ← heapAlloc (.listObj pathArr)
    ns := ns.insert "path" (.list pathRef)
    -- sys.modules as dict
    let mut modPairs : Array (Value × Value) := #[]
    for (k, v) in st.loadedModules do
      modPairs := modPairs.push (.str k, v)
    let modsRef ← heapAlloc (.dictObj modPairs)
    ns := ns.insert "modules" (.dict modsRef)
    -- sys.argv
    let argvRef ← heapAlloc (.listObj #[])
    ns := ns.insert "argv" (.list argvRef)
    -- sys.stdout / sys.stderr as TextIOWrapper instances
    let mkTextIO (streamName : String) : InterpM Value := do
      let mut attrs : Std.HashMap String Value := {}
      attrs := attrs.insert "_name" (.str streamName)
      let cls ← allocClassObj {
        name := "TextIOWrapper", bases := #[], mro := #[], ns := {}, slots := none }
      match cls with
      | .classObj cref => heapSetClassData cref {
          name := "TextIOWrapper", bases := #[], mro := #[cls], ns := {}, slots := none }
      | _ => pure ()
      let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
      return .instance instRef
    ns := ns.insert "stdout" (← mkTextIO "stdout")
    ns := ns.insert "stderr" (← mkTextIO "stderr")
    ns := ns.insert "stdin"  (← mkTextIO "stdin")
    -- sys.exit
    ns := ns.insert "exit" (.builtin "sys.exit")
    -- Constants
    ns := ns.insert "maxsize"      (.int 9223372036854775807)
    ns := ns.insert "version"      (.str "3.12.0 (LeanPython)")
    ns := ns.insert "platform"     (.str "linux")
    ns := ns.insert "byteorder"    (.str "little")
    ns := ns.insert "executable"   (.str "leanpython")
    ns := ns.insert "prefix"       (.str "/usr")
    ns := ns.insert "exec_prefix"  (.str "/usr")
    ns := ns.insert "version_info" (.tuple #[.int 3, .int 12, .int 0, .str "final", .int 0])
    ns := ns.insert "getrecursionlimit" (.builtin "sys.getrecursionlimit")
    ns := ns.insert "setrecursionlimit" (.builtin "sys.setrecursionlimit")
    some <$> mkMod ns
  | "os" => do
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "getcwd"  (.builtin "os.getcwd")
    ns := ns.insert "getenv"  (.builtin "os.getenv")
    ns := ns.insert "listdir" (.builtin "os.listdir")
    ns := ns.insert "sep"     (.str "/")
    ns := ns.insert "linesep" (.str "\n")
    ns := ns.insert "name"    (.str "posix")
    ns := ns.insert "curdir"  (.str ".")
    ns := ns.insert "pardir"  (.str "..")
    ns := ns.insert "extsep"  (.str ".")
    -- os.environ as empty dict (simplified)
    let envRef ← heapAlloc (.dictObj #[])
    ns := ns.insert "environ" (.dict envRef)
    -- os.path as submodule
    if let some pathMod ← getBuiltinModule "os.path" then
      ns := ns.insert "path" pathMod
    some <$> mkMod ns
  | "os.path" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "join"     (.builtin "os.path.join")
    ns := ns.insert "exists"   (.builtin "os.path.exists")
    ns := ns.insert "isfile"   (.builtin "os.path.isfile")
    ns := ns.insert "isdir"    (.builtin "os.path.isdir")
    ns := ns.insert "dirname"  (.builtin "os.path.dirname")
    ns := ns.insert "basename" (.builtin "os.path.basename")
    ns := ns.insert "abspath"  (.builtin "os.path.abspath")
    ns := ns.insert "splitext" (.builtin "os.path.splitext")
    ns := ns.insert "normpath" (.builtin "os.path.normpath")
    ns := ns.insert "sep"      (.str "/")
    some <$> mkMod ns
  | "time" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "time"      (.builtin "time.time")
    ns := ns.insert "monotonic" (.builtin "time.monotonic")
    ns := ns.insert "sleep"     (.builtin "time.sleep")
    some <$> mkMod ns
  | "datetime" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "datetime"  (.builtin "datetime.datetime")
    ns := ns.insert "timedelta" (.builtin "datetime.timedelta")
    ns := ns.insert "timezone"  (.builtin "datetime.timezone")
    ns := ns.insert "date"      (.builtin "datetime.date")
    ns := ns.insert "time"      (.builtin "datetime.time_cls")
    some <$> mkMod ns
  | "pathlib" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "Path"     (.builtin "pathlib.Path")
    ns := ns.insert "PurePath" (.builtin "pathlib.PurePath")
    some <$> mkMod ns
  | "logging" => do
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "getLogger"   (.builtin "logging.getLogger")
    ns := ns.insert "basicConfig" (.builtin "logging.basicConfig")
    ns := ns.insert "DEBUG"       (.int 10)
    ns := ns.insert "INFO"        (.int 20)
    ns := ns.insert "WARNING"     (.int 30)
    ns := ns.insert "ERROR"       (.int 40)
    ns := ns.insert "CRITICAL"    (.int 50)
    ns := ns.insert "Logger"      (.builtin "logging.Logger")
    ns := ns.insert "StreamHandler" (.builtin "logging.StreamHandler")
    ns := ns.insert "Formatter"   (.builtin "logging.Formatter")
    ns := ns.insert "NullHandler" (.builtin "logging.NullHandler")
    some <$> mkMod ns
  | "signal" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "signal"  (.builtin "signal.signal")
    ns := ns.insert "SIGINT"  (.int 2)
    ns := ns.insert "SIGTERM" (.int 15)
    ns := ns.insert "SIGKILL" (.int 9)
    ns := ns.insert "SIGHUP"  (.int 1)
    ns := ns.insert "SIG_DFL" (.int 0)
    ns := ns.insert "SIG_IGN" (.int 1)
    some <$> mkMod ns
  | "threading" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "Lock"  (.builtin "threading.Lock")
    ns := ns.insert "RLock" (.builtin "threading.RLock")
    ns := ns.insert "Event" (.builtin "threading.Event")
    some <$> mkMod ns
  | "tempfile" =>
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "mkdtemp"            (.builtin "tempfile.mkdtemp")
    ns := ns.insert "NamedTemporaryFile" (.builtin "tempfile.NamedTemporaryFile")
    some <$> mkMod ns
  | "pydantic" => do
    -- Create real BaseModel class object
    let baseModelCls ← allocClassObj {
      name := "BaseModel", bases := #[], mro := #[], ns := {}, slots := none }
    match baseModelCls with
    | .classObj ref => heapSetClassData ref {
        name := "BaseModel", bases := #[], mro := #[baseModelCls], ns := {}, slots := none }
    | _ => pure ()
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "BaseModel" baseModelCls
    ns := ns.insert "ConfigDict" (.builtin "pydantic.ConfigDict")
    ns := ns.insert "Field" (.builtin "pydantic.Field")
    ns := ns.insert "field_validator" (.builtin "pydantic.field_validator")
    ns := ns.insert "model_validator" (.builtin "pydantic.model_validator")
    ns := ns.insert "field_serializer" (.builtin "pydantic.field_serializer")
    ns := ns.insert "model_serializer" (.builtin "pydantic.model_serializer")
    some <$> mkMod ns
  | "pydantic.fields" => do
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "FieldInfo" (.builtin "pydantic.FieldInfo")
    some <$> mkMod ns
  | "pydantic.alias_generators" => do
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "to_camel" (.builtin "pydantic.alias_generators.to_camel")
    some <$> mkMod ns
  | "pydantic.annotated_handlers" => do
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "GetCoreSchemaHandler" (.builtin "pydantic.GetCoreSchemaHandler")
    some <$> mkMod ns
  | "pydantic_core" => do
    let mut ns : Std.HashMap String Value := {}
    if let some csMod ← getBuiltinModule "pydantic_core.core_schema" then
      ns := ns.insert "core_schema" csMod
    ns := ns.insert "CoreSchema" (.builtin "pydantic_core.CoreSchema")
    some <$> mkMod ns
  | "pydantic_core.core_schema" => do
    let mut ns : Std.HashMap String Value := {}
    ns := ns.insert "union_schema" (.builtin "core_schema.union_schema")
    ns := ns.insert "is_instance_schema" (.builtin "core_schema.is_instance_schema")
    ns := ns.insert "chain_schema" (.builtin "core_schema.chain_schema")
    ns := ns.insert "int_schema" (.builtin "core_schema.int_schema")
    ns := ns.insert "bool_schema" (.builtin "core_schema.bool_schema")
    ns := ns.insert "bytes_schema" (.builtin "core_schema.bytes_schema")
    ns := ns.insert "no_info_plain_validator_function"
      (.builtin "core_schema.no_info_plain_validator_function")
    ns := ns.insert "plain_serializer_function_ser_schema"
      (.builtin "core_schema.plain_serializer_function_ser_schema")
    some <$> mkMod ns
  | _ => return none

/-- Load a module by fully-qualified name and file path.
    Handles caching, circular import detection, and state isolation. -/
partial def loadModule (fqName : String) (filePath : String) (isPackage : Bool)
    : InterpM Value := do
  let st ← get
  -- 1. Check cache
  if let some v := st.loadedModules[fqName]? then return v
  -- 2. Circular import detection: return partial module
  if st.loadingModules.contains fqName then
    -- Allocate a partial (empty) module and cache it
    let md : ModuleData := {
      name := fqName, file := some filePath, package := none
      ns := {}, allNames := none }
    let modVal ← allocModule md
    modify fun s => { s with loadedModules := s.loadedModules.insert fqName modVal }
    return modVal
  -- 3. Mark as loading
  modify fun s => { s with loadingModules := s.loadingModules.insert fqName }
  -- 4. Load parent packages first
  let parts := fqName.splitOn "."
  if parts.length > 1 then
    for i in [1:parts.length] do
      let parentName := ".".intercalate (parts.take i)
      let st' ← get
      if st'.loadedModules[parentName]?.isNone then
        -- Try to load parent as package
        let parentResolved ← (resolveModulePath parentName st'.searchPaths : IO _)
        match parentResolved with
        | some (parentPath, parentIsPkg) =>
          let _ ← loadModule parentName parentPath parentIsPkg
        | none => pure ()  -- Parent might be implicit namespace package
  -- 5. Read and parse source
  let source ← (IO.FS.readFile filePath : IO String)
  let stmts ← match parse source with
    | .ok (.module ss) => pure ss
    | .error e => throwImportError s!"syntax error in {filePath}: {e}"
  -- 6. Save current context
  let saved ← get
  let savedGlobal := saved.globalScope
  let savedLocals := saved.localScopes
  let savedGlobalDecls := saved.globalDecls
  let savedNonlocalDecls := saved.nonlocalDecls
  let savedFile := saved.currentFile
  let savedPackage := saved.currentPackage
  -- 7. Set up fresh scope for module execution
  let pkg : Option String :=
    if isPackage then some fqName
    else if parts.length > 1 then some (".".intercalate (parts.take (parts.length - 1)))
    else none
  let mut moduleScope : Scope := {}
  moduleScope := moduleScope.insert "__name__" (.str fqName)
  moduleScope := moduleScope.insert "__file__" (.str filePath)
  match pkg with
  | some p => moduleScope := moduleScope.insert "__package__" (.str p)
  | none => pure ()
  set { saved with
    globalScope := moduleScope
    localScopes := []
    globalDecls := []
    nonlocalDecls := []
    currentFile := some filePath
    currentPackage := pkg }
  -- 8. Execute module body
  try
    execStmts stmts
  catch
    | .error e => do
      -- Restore context before propagating
      modify fun s => { s with
        globalScope := savedGlobal
        localScopes := savedLocals
        globalDecls := savedGlobalDecls
        nonlocalDecls := savedNonlocalDecls
        currentFile := savedFile
        currentPackage := savedPackage }
      throwRuntimeError e
    | other => do
      modify fun s => { s with
        globalScope := savedGlobal
        localScopes := savedLocals
        globalDecls := savedGlobalDecls
        nonlocalDecls := savedNonlocalDecls
        currentFile := savedFile
        currentPackage := savedPackage }
      throw other
  -- 9. Capture namespace
  let finalSt ← get
  let moduleNs := finalSt.globalScope
  -- 10. Extract __all__ if defined
  let allNames : Option (Array String) :=
    match moduleNs["__all__"]? with
    | some (.list _) => none  -- TODO: extract list elements
    | _ => none
  -- 11. Build module and cache
  let md : ModuleData := {
    name := fqName, file := some filePath, package := pkg
    ns := moduleNs, allNames := allNames }
  let modVal ← allocModule md
  modify fun s => { s with
    loadedModules := s.loadedModules.insert fqName modVal
    loadingModules := s.loadingModules.erase fqName }
  -- 12. Set as attribute on parent module if applicable
  if parts.length > 1 then
    let parentName := ".".intercalate (parts.take (parts.length - 1))
    let childName := parts.getLast!
    let st' ← get
    if let some (.module pref) := st'.loadedModules[parentName]? then
      let pmd ← heapGetModuleData pref
      heapSetModuleData pref { pmd with ns := pmd.ns.insert childName modVal }
  -- 13. Restore interpreter context
  modify fun s => { s with
    globalScope := savedGlobal
    localScopes := savedLocals
    globalDecls := savedGlobalDecls
    nonlocalDecls := savedNonlocalDecls
    currentFile := savedFile
    currentPackage := savedPackage }
  return modVal

/-- Resolve and load a module by name (builtin or file-based). -/
partial def resolveAndLoadModule (fqName : String) : InterpM Value := do
  -- Try builtin modules first
  if let some v ← getBuiltinModule fqName then return v
  -- Try file system
  let st ← get
  let resolved ← (resolveModulePath fqName st.searchPaths : IO _)
  match resolved with
  | some (path, isPkg) => loadModule fqName path isPkg
  | none => throwModuleNotFoundError fqName

-- ============================================================
-- Statement execution
-- ============================================================

partial def execStmts (stmts : List Stmt) : InterpM Unit := do
  for s in stmts do
    execStmt s

partial def execStmt (s : Stmt) : InterpM Unit := do
  match s with
  | .expr e _ => do let _ ← evalExpr e

  | .assign targets value _ => do
    let v ← evalExpr value
    for t in targets do assignToTarget t v

  | .augAssign target op value _ => do
    let current ← evalExpr target
    let rhs ← evalExpr value
    let result ← match current with
      | .instance _ =>
        match ← callDunder current (binOpToDunder op) [rhs] with
        | some r => pure r
        | none => evalBinOp op current rhs
      | _ => evalBinOp op current rhs
    assignToTarget target result

  | .annAssign target ann value _simple _ => do
    -- Record annotation in __annotations__ dict if in scope
    match target with
    | .name n _ => do
      let annVal ← evalExpr ann
      let annStr ← valueToStr annVal
      let annDict ← do
        try
          let existing ← lookupVariable "__annotations__"
          pure existing
        catch _ => pure .none
      match annDict with
      | .dict ref => do
        let pairs ← heapGetDict ref
        let key := Value.str n
        let mut newPairs := pairs
        let mut found := false
        for i in [:pairs.size] do
          if Value.beq pairs[i]!.1 key then
            newPairs := newPairs.set! i (key, .str annStr)
            found := true
            break
        if !found then newPairs := newPairs.push (key, .str annStr)
        heapSetDict ref newPairs
      | _ => pure ()
      -- Also store raw annotation value for Pydantic core_schema hooks
      let rawDict ← do
        try
          let existing ← lookupVariable "__annotations_raw__"
          pure existing
        catch _ => pure .none
      match rawDict with
      | .dict ref => do
        let pairs ← heapGetDict ref
        let key := Value.str n
        let mut newPairs := pairs
        let mut found := false
        for i in [:pairs.size] do
          if Value.beq pairs[i]!.1 key then
            newPairs := newPairs.set! i (key, annVal)
            found := true
            break
        if !found then newPairs := newPairs.push (key, annVal)
        heapSetDict ref newPairs
      | _ => pure ()
    | _ => pure ()
    if let some valExpr := value then
      assignToTarget target (← evalExpr valExpr)

  | .return_ expr _ => do
    let v ← match expr with
      | some e => evalExpr e
      | none => pure .none
    throwReturn v

  | .if_ test body orelse _ => do
    if ← isTruthy (← evalExpr test) then execStmts body
    else execStmts orelse

  | .while_ test body orelse _ => do
    let mut brokeOut := false
    repeat do
      if !(← isTruthy (← evalExpr test)) then break
      try execStmts body
      catch
      | .control .break_ => brokeOut := true; break
      | .control .continue_ => pure ()
      | other => throw other
    if !brokeOut then execStmts orelse

  | .for_ target iter body orelse _ => do
    let items ← iterValues (← evalExpr iter)
    let mut brokeOut := false
    for item in items do
      assignToTarget target item
      try execStmts body
      catch
      | .control .break_ => brokeOut := true; break
      | .control .continue_ => pure ()
      | other => throw other
    if !brokeOut then execStmts orelse

  | .functionDef fd => do
    let defaults ← fd.args.defaults.mapM evalExpr
    let kwDefaults ← fd.args.kwDefaults.mapM fun
      | some e => do return some (← evalExpr e)
      | none => return none
    let st ← get
    let funcVal ← allocFunc (FuncData.mk fd.name fd.args fd.body defaults.toArray kwDefaults.toArray st.localScopes.toArray (stmtsContainYield fd.body))
    -- Apply decorators (innermost first = reverse the list)
    let mut decorated := funcVal
    for dec in fd.decoratorList.reverse do
      let decVal ← evalExpr dec
      decorated ← callValueDispatch decVal [decorated] []
    setVariable fd.name decorated

  | .asyncFunctionDef fd => do
    -- Treat async as regular for now
    let defaults ← fd.args.defaults.mapM evalExpr
    let kwDefaults ← fd.args.kwDefaults.mapM fun
      | some e => do return some (← evalExpr e)
      | none => return none
    let st ← get
    let funcVal ← allocFunc (FuncData.mk fd.name fd.args fd.body defaults.toArray kwDefaults.toArray st.localScopes.toArray (stmtsContainYield fd.body))
    -- Apply decorators
    let mut decorated := funcVal
    for dec in fd.decoratorList.reverse do
      let decVal ← evalExpr dec
      decorated ← callValueDispatch decVal [decorated] []
    setVariable fd.name decorated

  | .pass_ _ => return
  | .break_ _ => throwBreak
  | .continue_ _ => throwContinue

  | .assert_ test msg _ => do
    if !(← isTruthy (← evalExpr test)) then
      let msgStr ← match msg with
        | some e => valueToStr (← evalExpr e)
        | none => pure ""
      throwAssertionError msgStr

  | .delete targets _ => do
    for t in targets do
      match t with
      | .name n _ => deleteVariable n
      | .subscript obj idx _ => do
        let objVal ← evalExpr obj
        let idxVal ← evalExpr idx
        deleteSubscriptValue objVal idxVal
      | .attribute obj attr _ => do
        let objVal ← evalExpr obj
        match objVal with
        | .instance iref => do
          let id_ ← heapGetInstanceData iref
          -- Check for __delattr__ hook
          let mut delattrFound : Option (Value × Value) := none
          match id_.cls with
          | .classObj cref => do
            let cd ← heapGetClassData cref
            for mroEntry in cd.mro do
              if delattrFound.isSome then break
              match mroEntry with
              | .classObj mref => do
                let mcd ← heapGetClassData mref
                if let some fn := mcd.ns["__delattr__"]? then
                  delattrFound := some (fn, mroEntry)
              | _ => pure ()
          | _ => pure ()
          match delattrFound with
          | some (fn, definingCls) =>
            match fn with
            | .function fref => do
              let fd ← heapGetFunc fref
              let scope ← bindFuncParams fd.params [objVal, .str attr] [] fd.defaults fd.kwDefaults
              let scopeWithClass := scope.insert "__class__" definingCls
              let _ ← callRegularFunc fd scopeWithClass
            | _ => let _ ← callValueDispatch fn [objVal, .str attr] []
          | none =>
            heapSetInstanceData iref { id_ with attrs := id_.attrs.erase attr }
        | .classObj cref => do
          let cd ← heapGetClassData cref
          heapSetClassData cref { cd with ns := cd.ns.erase attr }
        | _ => throwRuntimeError (.runtimeError "cannot delete attribute")
      | _ => throwRuntimeError (.runtimeError "invalid delete target")

  | .global_ names _ => do
    for n in names do declareGlobal n

  | .nonlocal_ names _ => do
    for n in names do declareNonlocal n

  | .classDef cd => do
    -- Evaluate base classes
    let bases ← cd.bases.mapM evalExpr
    -- Execute class body in a new scope
    pushScope {}
    -- Initialize __annotations__ dict for the class body
    let annRef ← heapAlloc (.dictObj #[])
    setVariable "__annotations__" (.dict annRef)
    -- Initialize __annotations_raw__ dict for Pydantic core_schema hooks
    let annRawRef ← heapAlloc (.dictObj #[])
    setVariable "__annotations_raw__" (.dict annRawRef)
    try execStmts cd.body
    catch
    | .control _ => pure ()
    | other => do popScope; throw other
    let st ← get
    let classScope := match st.localScopes with
      | s :: _ => s
      | [] => ({} : Scope)
    popScope
    -- Build namespace from class body scope
    let mut ns : Std.HashMap String Value := {}
    for (k, v) in classScope do
      ns := ns.insert k v
    -- Check for __slots__
    let slotsOpt ← match ns["__slots__"]? with
    | some (.list ref) => do
      let items ← heapGetList ref
      let names ← items.toList.mapM fun v =>
        match v with
        | .str s => pure s
        | _ => throwTypeError "__slots__ items must be strings"
      pure (some names.toArray)
    | some (.tuple items) => do
      let names ← items.toList.mapM fun v =>
        match v with
        | .str s => pure s
        | _ => throwTypeError "__slots__ items must be strings"
      pure (some names.toArray)
    | none => pure none
    | _ => pure none
    -- Allocate the class with placeholder MRO
    let classVal ← allocClassObj { name := cd.name, bases := bases.toArray, mro := #[], ns := ns, slots := slotsOpt }
    -- Compute C3 MRO and fix up
    let mro ← computeC3Mro classVal bases.toArray
    match classVal with
    | .classObj cref => do
      let cd' ← heapGetClassData cref
      heapSetClassData cref { cd' with mro := mro }
    | _ => pure ()
    -- Post-process Pydantic BaseModel subclasses
    let isPydanticModel ← isBaseModelSubclass bases.toArray
    if isPydanticModel then
      match classVal with
      | .classObj cref => applyPydanticModelProcessing classVal cref bases.toArray
      | _ => pure ()
    -- Post-process enum classes: convert simple assignments to enum members
    let isEnum ← isEnumSubclass bases.toArray
    if isEnum then
      match classVal with
      | .classObj cref => do
        let cd' ← heapGetClassData cref
        let mut ns' := cd'.ns
        let mut autoCounter := 1
        -- Extract names from class body in source order
        let memberNames := cd.body.filterMap fun stmt =>
          match stmt with
          | .assign [.name n _] _ _ => if n.startsWith "__" && n.endsWith "__" then none else some n
          | .annAssign (.name n _) _ _ _ _ => if n.startsWith "__" && n.endsWith "__" then none else some n
          | _ => none
        for k in memberNames do
          if let some v := cd'.ns[k]? then
            match v with
            | .function _ | .staticMethod _ | .classMethod _ | .property _ _ _ => continue
            | _ =>
              let memberVal ← match v with
                | .builtin "enum.auto_value" => do
                  let val := Value.int autoCounter
                  autoCounter := autoCounter + 1
                  pure val
                | _ => pure v
              let member ← allocInstance {
                cls := classVal
                attrs := ({} : Std.HashMap String Value)
                  |>.insert "name" (.str k)
                  |>.insert "value" memberVal
                  |>.insert "_name_" (.str k)
                  |>.insert "_value_" memberVal
              }
              ns' := ns'.insert k member
        heapSetClassData cref { cd' with ns := ns' }
      | _ => pure ()
    -- Apply class decorators (innermost first = reverse the list)
    let mut decorated := classVal
    for dec in cd.decoratorList.reverse do
      let decVal ← evalExpr dec
      decorated ← callValueDispatch decVal [decorated] []
    setVariable cd.name decorated

  | .raise_ exprOpt cause _ => do
    -- Evaluate cause if present (for validation; chaining info not stored yet)
    if let some causeExpr := cause then
      let causeVal ← evalExpr causeExpr
      match causeVal with
      | .exception _ _ => pure ()
      | .none => pure ()
      | .builtin _ => pure ()
      | _ => throwTypeError "exception cause must be None or derive from BaseException"
    match exprOpt with
    | some e => do
      let v ← evalExpr e
      match v with
      | .exception typeName msg =>
        -- Map exception type to the appropriate RuntimeError variant
        let err := match typeName with
          | "ValueError" => RuntimeError.valueError msg
          | "TypeError" => RuntimeError.typeError msg
          | "KeyError" => RuntimeError.keyError msg
          | "IndexError" => RuntimeError.indexError msg
          | "NameError" => RuntimeError.nameError msg
          | "ZeroDivisionError" => RuntimeError.zeroDivision msg
          | "AssertionError" => RuntimeError.assertionError msg
          | "AttributeError" => RuntimeError.attributeError msg
          | "OverflowError" => RuntimeError.overflowError msg
          | "StopIteration" => RuntimeError.stopIteration
          | "NotImplementedError" => RuntimeError.notImplemented msg
          | _ => RuntimeError.runtimeError msg
        throwRuntimeError err
      | .builtin name =>
        -- raise ValueError (no args)
        if isBuiltinName name then
          throwRuntimeError (.runtimeError name)
        else
          throwTypeError "exceptions must derive from BaseException"
      | _ => throwTypeError "exceptions must derive from BaseException"
    | none => do
      -- Bare raise: re-raise active exception
      let st ← get
      match st.activeException with
      | some e => throwRuntimeError e
      | none => throwRuntimeError (.runtimeError "No active exception to re-raise")

  | .try_ body handlers orelse finally_ _ => do
    let bodyResult ← do
      try
        execStmts body
        pure (none : Option Signal)
      catch
      | sig@(.error _) => pure (some sig)
      | other => throw other
    match bodyResult with
    | none => do
      try execStmts orelse catch e => do execStmts finally_; throw e
      execStmts finally_
    | some (.error e) => do
      -- Save the active exception for bare `raise`
      modify fun st => { st with activeException := some e }
      let errorTypeName := runtimeErrorTypeName e
      let errorMsg := runtimeErrorMessage e
      let mut handled := false
      for handler in handlers do
        match handler with
        | .mk type_ handlerName handlerBody _ => do
          let doesMatch ← match type_ with
            | none => pure true  -- bare `except:` catches everything
            | some typeExpr => do
              let typeVal ← evalExpr typeExpr
              match typeVal with
              | .builtin typN => pure (exceptionMatches errorTypeName typN)
              | .tuple typeVals => do
                -- except (TypeError, ValueError): ...
                let mut found := false
                for tv in typeVals do
                  match tv with
                  | .builtin typN =>
                    if exceptionMatches errorTypeName typN then found := true; break
                  | _ => pure ()
                pure found
              | _ => pure false
          if doesMatch then
            if let some n := handlerName then
              setVariable n (.exception errorTypeName errorMsg)
            try execStmts handlerBody catch exc => do execStmts finally_; throw exc
            handled := true
            break
      if !handled then do execStmts finally_; throwRuntimeError e
      else execStmts finally_
      -- Clear active exception
      modify fun st => { st with activeException := none }
    | some other => do execStmts finally_; throw other

  | .with_ items body _ => do
    -- Helper: try to get a named attribute from an object
    let getAttr := fun (obj : Value) (name : String) => do
      try
        let v ← getAttributeValue obj name
        return some v
      catch
      | .error (.attributeError _) => return (none : Option Value)
      | other => throw other
    -- Evaluate context managers and call __enter__
    let mut managers : List (Value × Value) := []
    for item in items do
      let mgr ← evalExpr item.contextExpr
      let entered ← match ← getAttr mgr "__enter__" with
        | some fn => callValueDispatch fn [mgr] []
        | none => pure mgr
      if let some target := item.optionalVars then
        assignToTarget target entered
      managers := managers ++ [(mgr, entered)]
    -- Execute body with cleanup
    let mut bodyError : Option Signal := none
    try execStmts body
    catch
    | sig@(.error _) => bodyError := some sig
    | other => do
      for (mgr, _) in managers.reverse do
        if let some fn ← getAttr mgr "__exit__" then
          let _ ← callValueDispatch fn [mgr, .none, .none, .none] []
      throw other
    -- Call __exit__ on each manager in reverse order
    let mut suppressed := false
    for (mgr, _) in managers.reverse do
      if let some fn ← getAttr mgr "__exit__" then
        let exitArgs := match bodyError with
          | none => [mgr, .none, .none, .none]
          | some (.error e) =>
            let excType := Value.str (runtimeErrorTypeName e)
            let excVal := Value.exception (runtimeErrorTypeName e) (runtimeErrorMessage e)
            [mgr, excType, excVal, .none]
          | _ => [mgr, .none, .none, .none]
        let result ← callValueDispatch fn exitArgs []
        if ← isTruthy result then suppressed := true
    -- Re-raise body error if not suppressed
    match bodyError with
    | some sig => if !suppressed then throw sig
    | none => pure ()
  | .import_ aliases _ => do
    for alias in aliases do
      let fqName := alias.name
      let modVal ← resolveAndLoadModule fqName
      match alias.asName with
      | some asName => setVariable asName modVal
      | none =>
        -- "import foo.bar.baz" binds "foo" in the local scope
        let topName := (fqName.splitOn ".").head!
        if topName != fqName then
          -- Load and bind the top-level module
          let topMod ← resolveAndLoadModule topName
          setVariable topName topMod
        else
          setVariable topName modVal

  | .importFrom modNameOpt aliases levelOpt _ => do
    -- Handle `from __future__ import annotations` as no-op
    if modNameOpt == some "__future__" && levelOpt.isNone then
      return
    -- Resolve the full module name
    let fqName ← match levelOpt with
      | some level => do
        let st ← get
        match resolveRelativeImport st.currentPackage level modNameOpt with
        | .ok name => pure name
        | .error msg => throwImportError msg
      | none =>
        match modNameOpt with
        | some name => pure name
        | none => throwImportError "no module name in import"
    -- Load the module
    let modVal ← resolveAndLoadModule fqName
    let modRef ← match modVal with
      | .module ref => pure ref
      | _ => throwImportError s!"expected module object for '{fqName}'"
    let md ← heapGetModuleData modRef
    -- Import names from module namespace
    for alias in aliases do
      if alias.name == "*" then
        -- from module import *
        let names := match md.allNames with
          | some all => all.toList
          | none => md.ns.toList.map fun (p : String × Value) => p.1
        for name in names do
          if !name.startsWith "_" || md.allNames.isSome then
            match md.ns[name]? with
            | some v => setVariable name v
            | none => pure ()
      else
        -- from module import name [as alias]
        match md.ns[alias.name]? with
        | some v =>
          let bindName := alias.asName.getD alias.name
          setVariable bindName v
        | none => do
          -- The name might be a subpackage/submodule
          let subFqName := fqName ++ "." ++ alias.name
          try
            let subMod ← resolveAndLoadModule subFqName
            let bindName := alias.asName.getD alias.name
            setVariable bindName subMod
          catch _ =>
            throwImportError s!"cannot import name '{alias.name}' from '{fqName}'"

  | .asyncFor target iter body orelse _ => do
    let items ← iterValues (← evalExpr iter)
    let mut brokeOut := false
    for item in items do
      assignToTarget target item
      try execStmts body
      catch
      | .control .break_ => brokeOut := true; break
      | .control .continue_ => pure ()
      | other => throw other
    if !brokeOut then execStmts orelse

  | .asyncWith _items _body _ => throwNotImplemented "async with"

partial def deleteSubscriptValue (obj idx : Value) : InterpM Unit := do
  match obj with
  | .list ref =>
    match idx with
    | .int i => do
      let arr ← heapGetList ref
      let ni := normalizeIndex i arr.size
      if ni < 0 || ni >= arr.size then throwIndexError "pop index out of range"
      heapSetList ref (arr.toList.take ni.toNat ++ arr.toList.drop (ni.toNat + 1) |>.toArray)
    | _ => throwTypeError "list indices must be integers"
  | .dict ref => do
    let pairs ← heapGetDict ref
    let mut newPairs : Array (Value × Value) := #[]
    let mut found := false
    for (k, v) in pairs do
      if !found && (← valueEq k idx) then found := true
      else newPairs := newPairs.push (k, v)
    if !found then throwKeyError (← valueRepr idx)
    heapSetDict ref newPairs
  | .instance _ => do
    match ← callDunder obj "__delitem__" [idx] with
    | some _ => return ()
    | none => throwTypeError s!"'{typeName obj}' does not support item deletion"
  | _ => throwTypeError s!"'{typeName obj}' does not support item deletion"

-- ============================================================
-- Method call helpers (called by callValueDispatch)
-- ============================================================

-- ============================================================
-- Generator method dispatch
-- ============================================================

partial def callGeneratorMethod (ref : HeapRef) (method : String)
    (_args : List Value) : InterpM Value := do
  match method with
  | "__next__" => do
    let (buf, idx) ← heapGetGenerator ref
    if idx >= buf.size then throwRuntimeError .stopIteration
    heapSetGeneratorIdx ref (idx + 1)
    return buf[idx]!
  | "__iter__" => return .generator ref
  | "close" => do
    let (buf, _) ← heapGetGenerator ref
    heapSetGeneratorIdx ref buf.size
    return .none
  | _ => throwAttributeError s!"'generator' object has no attribute '{method}'"

-- ============================================================
-- Bound method dispatch
-- ============================================================

partial def callBoundMethod (receiver : Value) (method : String) (args : List Value)
    (kwargs : List (String × Value) := []) : InterpM Value := do
  match receiver with
  | .list ref => callListMethod ref method args
  | .dict ref => callDictMethod ref method args
  | .str s => callStrMethod s method args
  | .set ref => callSetMethod ref method args
  | .int n => callIntMethod n method args
  | .bytes b => callBytesMethod b method args
  | .tuple arr => callTupleMethod arr method args
  | .builtin name => callBuiltinTypeMethod name method args
  | .generator ref => callGeneratorMethod ref method args
  | .property getter setter deleter =>
    match method with
    | "setter" => match args with
      | [fn] => return .property getter (some fn) deleter
      | _ => throwTypeError "setter() takes one argument"
    | "getter" => match args with
      | [fn] => return .property fn setter deleter
      | _ => throwTypeError "getter() takes one argument"
    | "deleter" => match args with
      | [fn] => return .property getter setter (some fn)
      | _ => throwTypeError "deleter() takes one argument"
    | _ => throwAttributeError s!"'property' object has no attribute '{method}'"
  | .classObj cref => do
    -- Class-level method call (e.g., MyClass.method() or bound classmethod)
    let cd ← heapGetClassData cref
    let mut found : Option Value := none
    for mroEntry in cd.mro do
      if found.isSome then break
      match mroEntry with
      | .classObj mref => do
        let mcd ← heapGetClassData mref
        if let some fn := mcd.ns[method]? then
          found := some fn
      | _ => pure ()
    match found with
    | some (.classMethod innerFn) =>
      callValueDispatch innerFn (receiver :: args) []
    | some (.staticMethod innerFn) =>
      callValueDispatch innerFn args []
    | some (.function fref) => do
      -- Regular function called on class (no self binding)
      let fd ← heapGetFunc fref
      let scope ← bindFuncParams fd.params args [] fd.defaults fd.kwDefaults
      callRegularFunc fd scope
    | some fn => callValueDispatch fn args []
    | none => throwAttributeError s!"type object '{cd.name}' has no attribute '{method}'"
  | .instance iref => do
    -- Look up method in instance's class MRO and call with self
    let id_ ← heapGetInstanceData iref
    -- Check for builtin instance types (BytesIO, StringIO)
    match id_.cls with
    | .classObj cref => do
      let cd_ ← heapGetClassData cref
      if cd_.name == "BytesIO" then return ← callBytesIOMethod iref method args
      if cd_.name == "StringIO" then return ← callStringIOMethod iref method args
      if cd_.name == "SHA256Hash" || cd_.name == "SHAKE128Hash" then
        return ← callHashMethod iref method args
      if cd_.name == "HMAC" then return ← callHmacMethod iref method args
      if cd_.name == "TextIOWrapper" then return ← callTextIOWrapperMethod iref method args
      if cd_.name == "Datetime" then return ← callDatetimeMethod iref method args
      if cd_.name == "Timedelta" then return ← callTimedeltaMethod iref method args
      if cd_.name == "Timezone" then return ← callTimezoneMethod iref method args
      if cd_.name == "PurePath" then return ← callPathMethod iref method args
      if cd_.name == "Logger" then return ← callLoggerMethod iref method args
      -- Logging handler/formatter stubs (all methods are no-ops)
      if cd_.name == "StreamHandler" || cd_.name == "NullHandler" ||
            cd_.name == "Formatter" then
        return .none
      -- Threading Lock/RLock stubs
      if cd_.name == "Lock" || cd_.name == "RLock" then
        match method with
        | "acquire" | "__enter__" => return .bool true
        | "release" | "__exit__" => return .none
        | "locked" => return .bool false
        | _ => throwAttributeError s!"'{cd_.name}' object has no attribute '{method}'"
      -- Threading Event stubs
      if cd_.name == "Event" then
        match method with
        | "set" | "clear" => return .none
        | "is_set" => return .bool false
        | "wait" => return .bool true
        | _ => throwAttributeError s!"'Event' object has no attribute '{method}'"
      -- NamedTemporaryFile stubs
      if cd_.name == "NamedTemporaryFile" then
        match method with
        | "write" | "read" | "close" => return .none
        | "__enter__" => return .instance iref
        | "__exit__" => return .none
        | _ => throwAttributeError s!"'NamedTemporaryFile' object has no attribute '{method}'"
      -- Pydantic model methods (model_copy, model_dump)
      if cd_.ns["__pydantic_model__"]? == some (.bool true) then
        match method with
        | "model_copy" => return ← callPydanticModelCopy receiver iref cref args kwargs
        | "model_dump" => return ← callPydanticModelDump iref cref args kwargs
        | _ => pure ()  -- fall through to normal MRO lookup
      let cd ← heapGetClassData cref
      let mut found : Option (Value × Value) := none  -- (function, defining class)
      for mroEntry in cd.mro do
        if found.isSome then break
        match mroEntry with
        | .classObj mref => do
          let mcd ← heapGetClassData mref
          if let some fn := mcd.ns[method]? then
            found := some (fn, mroEntry)
        | _ => pure ()
      match found with
      | some (fn, definingCls) =>
        match fn with
        | .classMethod innerFn =>
          -- Call with class as first arg instead of instance
          callValueDispatch innerFn (id_.cls :: args) []
        | .staticMethod innerFn =>
          -- Call without self
          callValueDispatch innerFn args []
        | .function fref => do
          let fd ← heapGetFunc fref
          let scope ← bindFuncParams fd.params (receiver :: args) [] fd.defaults fd.kwDefaults
          let scopeWithClass := scope.insert "__class__" definingCls
          callRegularFunc fd scopeWithClass
        | _ => callValueDispatch fn (receiver :: args) []
      | none => throwAttributeError s!"'{cd.name}' object has no attribute '{method}'"
    | _ => throwAttributeError s!"instance has no attribute '{method}'"
  | .superObj startAfterCls inst => do
    -- Look up method in MRO starting after startAfterCls
    let instCls ← match inst with
      | .instance iref => do
        let id_ ← heapGetInstanceData iref
        pure id_.cls
      | _ => pure Value.none
    match instCls with
    | .classObj cref => do
      let cd ← heapGetClassData cref
      let mut pastStart := false
      let mut found : Option (Value × Value) := none  -- (function, defining class)
      for mroEntry in cd.mro do
        if found.isSome then break
        if !pastStart then
          if Value.beq mroEntry startAfterCls then pastStart := true
          continue
        match mroEntry with
        | .classObj mref => do
          let mcd ← heapGetClassData mref
          if let some fn := mcd.ns[method]? then
            found := some (fn, mroEntry)
        | _ => pure ()
      match found with
      | some (fn, definingCls) =>
        match fn with
        | .function fref => do
          let fd ← heapGetFunc fref
          let scope ← bindFuncParams fd.params (inst :: args) [] fd.defaults fd.kwDefaults
          let scopeWithClass := scope.insert "__class__" definingCls
          callRegularFunc fd scopeWithClass
        | _ => callValueDispatch fn (inst :: args) []
      | none => throwAttributeError s!"'super' object has no attribute '{method}'"
    | _ => throwAttributeError s!"'super' object has no attribute '{method}'"
  | _ => throwAttributeError s!"'{typeName receiver}' object has no attribute '{method}'"

-- ============================================================
-- map / filter (need callValueDispatch, so must be in mutual block)
-- ============================================================

partial def builtinMap (args : List Value) : InterpM Value := do
  match args with
  | [func, iter] => do
    let items ← iterValues iter
    let mut result : Array Value := #[]
    for item in items do
      let v ← callValueDispatch func [item] []
      result := result.push v
    allocList result
  | _ => throwTypeError "map() requires at least two arguments"

partial def builtinFilter (args : List Value) : InterpM Value := do
  match args with
  | [func, iter] => do
    let items ← iterValues iter
    let mut result : Array Value := #[]
    for item in items do
      let keep ← match func with
        | .none => isTruthy item
        | _ => do isTruthy (← callValueDispatch func [item] [])
      if keep then result := result.push item
    allocList result
  | _ => throwTypeError "filter() requires exactly two arguments"

-- ============================================================
-- Enum support: check if any base class is an Enum
-- ============================================================

partial def isEnumSubclass (bases : Array Value) : InterpM Bool := do
  for base in bases do
    match base with
    | .classObj ref => do
      let cd ← heapGetClassData ref
      if cd.name == "Enum" || cd.name == "IntEnum" then return true
      -- Also check MRO
      for mroEntry in cd.mro do
        match mroEntry with
        | .classObj mref => do
          let mcd ← heapGetClassData mref
          if mcd.name == "Enum" || mcd.name == "IntEnum" then return true
        | _ => pure ()
    | _ => pure ()
  return false

-- ============================================================
-- Pydantic support: check if any base class is a BaseModel
-- ============================================================

partial def isBaseModelSubclass (bases : Array Value) : InterpM Bool := do
  for base in bases do
    match base with
    | .classObj ref => do
      let cd ← heapGetClassData ref
      if cd.name == "BaseModel" then return true
      for mroEntry in cd.mro do
        match mroEntry with
        | .classObj mref => do
          let mcd ← heapGetClassData mref
          if mcd.name == "BaseModel" then return true
        | _ => pure ()
    | _ => pure ()
  return false

-- ============================================================
-- functools.reduce (needs callValueDispatch for callback)
-- ============================================================

partial def builtinFunctoolsReduce (args : List Value) : InterpM Value := do
  match args with
  | [func, iter] => do
    let items ← iterValues iter
    if items.isEmpty then throwTypeError "reduce() of empty iterable with no initial value"
    let mut acc := items[0]!
    for i in [1:items.size] do
      acc ← callValueDispatch func [acc, items[i]!] []
    return acc
  | [func, iter, initial] => do
    let items ← iterValues iter
    let mut acc := initial
    for item in items do
      acc ← callValueDispatch func [acc, item] []
    return acc
  | _ => throwTypeError "reduce() requires 2 or 3 arguments"

-- ============================================================
-- itertools.accumulate (needs callValueDispatch for custom func)
-- ============================================================

partial def builtinItertoolsAccumulate (args : List Value)
    (kwargs : List (String × Value)) : InterpM Value := do
  let (iter, func) := match args with
    | [iter] => (iter, none)
    | [iter, func] => (iter, some func)
    | _ => (Value.none, none)
  let func := match kwargs.find? (fun p => p.1 == "func") with
    | some (_, f) => some f
    | none => func
  let initial := kwargs.find? (fun p => p.1 == "initial") |>.map (·.2)
  let items ← iterValues iter
  let mut result : Array Value := #[]
  let mut acc : Value := match initial with
    | some v => v
    | none =>
      if items.isEmpty then .none
      else items[0]!
  match initial with
  | some _ =>
    result := result.push acc
    for item in items do
      acc ← match func with
        | some f => callValueDispatch f [acc, item] []
        | none => evalBinOp .add acc item
      result := result.push acc
  | none =>
    if items.isEmpty then return ← allocList #[]
    result := result.push acc
    for i in [1:items.size] do
      acc ← match func with
        | some f => callValueDispatch f [acc, items[i]!] []
        | none => evalBinOp .add acc items[i]!
      result := result.push acc
  allocList result

-- ============================================================
-- hasattr / getattr (need getAttributeValue, so must be in mutual block)
-- ============================================================

partial def builtinHasattr (args : List Value) : InterpM Value := do
  match args with
  | [obj, .str name] => do
    try
      let _ ← getAttributeValue obj name
      return .bool true
    catch
    | .error (.attributeError _) => return .bool false
    | other => throw other
  | _ => throwTypeError "hasattr() takes 2 arguments"

partial def builtinGetattr (args : List Value) : InterpM Value := do
  match args with
  | [obj, .str name] => getAttributeValue obj name
  | [obj, .str name, default_] => do
    try
      getAttributeValue obj name
    catch
    | .error (.attributeError _) => return default_
    | other => throw other
  | _ => throwTypeError "getattr() takes 2 or 3 arguments"

-- ============================================================
-- setattr builtin
-- ============================================================

partial def builtinSetattr (args : List Value) : InterpM Value := do
  match args with
  | [obj, .str name, value] => do
    match obj with
    | .instance iref => do
      let id_ ← heapGetInstanceData iref
      heapSetInstanceData iref { id_ with attrs := id_.attrs.insert name value }
      return .none
    | .classObj cref => do
      let cd ← heapGetClassData cref
      heapSetClassData cref { cd with ns := cd.ns.insert name value }
      return .none
    | _ => throwTypeError s!"'{typeName obj}' object attribute '{name}' is read-only"
  | _ => throwTypeError "setattr() takes exactly 3 arguments"

-- ============================================================
-- Int methods
-- ============================================================

partial def callIntMethod (n : Int) (method : String) (args : List Value)
    : InterpM Value := do
  match method with
  | "bit_length" =>
    match args with
    | [] =>
      if n == 0 then return .int 0
      else return .int (Nat.log2 n.natAbs + 1)
    | _ => throwTypeError "bit_length() takes no arguments"
  | "to_bytes" =>
    match args with
    | [.int length, .str byteorder] => intToBytes n length.toNat byteorder
    | _ => throwTypeError "to_bytes() takes length and byteorder arguments"
  | _ => throwAttributeError s!"'int' object has no attribute '{method}'"
where
  intToBytes (n : Int) (length : Nat) (byteorder : String) : InterpM Value := do
    let val := n.natAbs
    let mut result := ByteArray.empty
    for _ in [:length] do result := result.push 0
    let mut v := val
    for i in [:length] do
      let byteIdx := if byteorder == "big" then length - 1 - i else i
      result := result.set! byteIdx (v % 256).toUInt8
      v := v / 256
    return .bytes result

-- ============================================================
-- Bytes methods
-- ============================================================

partial def callBytesMethod (b : ByteArray) (method : String) (args : List Value)
    : InterpM Value := do
  match method with
  | "hex" =>
    match args with
    | [] => do
      let mut result := ""
      for byte in b.toList do
        let hi := byte.toNat / 16
        let lo := byte.toNat % 16
        let hexDigit (n : Nat) : Char := if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
        result := result ++ String.ofList [hexDigit hi, hexDigit lo]
      return .str result
    | _ => throwTypeError "hex() takes no arguments"
  | "decode" =>
    match args with
    | [] => return .str (String.ofList (b.toList.map (fun byte => Char.ofNat byte.toNat)))
    | [.str _encoding] => return .str (String.ofList (b.toList.map (fun byte => Char.ofNat byte.toNat)))
    | _ => throwTypeError "decode() takes at most 1 argument"
  | _ => throwAttributeError s!"'bytes' object has no attribute '{method}'"

-- ============================================================
-- Tuple methods
-- ============================================================

partial def callTupleMethod (arr : Array Value) (method : String) (args : List Value)
    : InterpM Value := do
  match method with
  | "count" =>
    match args with
    | [v] => do
      let mut count : Nat := 0
      for elem in arr do
        if ← valueEq elem v then count := count + 1
      return .int count
    | _ => throwTypeError "count() takes exactly one argument"
  | "index" =>
    match args with
    | [v] => do
      for i in [:arr.size] do
        if ← valueEq arr[i]! v then return .int i
      throwValueError "tuple.index(x): x not in tuple"
    | _ => throwTypeError "index() takes 1 argument"
  | _ => throwAttributeError s!"'tuple' object has no attribute '{method}'"

-- ============================================================
-- Builtin type methods (int.from_bytes, bytes.fromhex, etc.)
-- ============================================================

partial def callBuiltinTypeMethod (typeName_ : String) (method : String) (args : List Value)
    : InterpM Value := do
  match typeName_, method with
  | "int", "from_bytes" =>
    match args with
    | [.bytes data, .str byteorder] => do
      let mut result : Nat := 0
      if byteorder == "big" then
        for byte in data.toList do
          result := result * 256 + byte.toNat
      else
        let mut shift : Nat := 0
        for byte in data.toList do
          result := result + byte.toNat * (256 ^ shift)
          shift := shift + 1
      return .int result
    | _ => throwTypeError "int.from_bytes() takes bytes and byteorder arguments"
  | "bytes", "fromhex" =>
    match args with
    | [.str hexStr] => do
      let chars := hexStr.toList.filter (· != ' ')
      if chars.length % 2 != 0 then
        throwValueError "non-hexadecimal number found in fromhex() arg"
      let mut result := ByteArray.empty
      let mut i : Nat := 0
      while i + 1 < chars.length do
        let hi ← hexCharToNat chars[i]!
        let lo ← hexCharToNat chars[i + 1]!
        result := result.push (hi * 16 + lo).toUInt8
        i := i + 2
      return .bytes result
    | _ => throwTypeError "fromhex() takes exactly 1 argument"
  | "object", "__new__" =>
    -- object.__new__(cls) — allocate a bare instance of cls
    match args with
    | [cls@(.classObj _)] => allocInstance { cls := cls, attrs := {} }
    | _ => throwTypeError "object.__new__(cls): cls must be a type"
  | "object", "__setattr__" =>
    -- object.__setattr__(self, name, value) — direct attribute store
    match args with
    | [.instance iref, .str name, value] => do
      let id_ ← heapGetInstanceData iref
      heapSetInstanceData iref { id_ with attrs := id_.attrs.insert name value }
      return .none
    | _ => throwTypeError "object.__setattr__() takes 3 arguments"
  | "object", "__delattr__" =>
    -- object.__delattr__(self, name) — direct attribute delete
    match args with
    | [.instance iref, .str name] => do
      let id_ ← heapGetInstanceData iref
      heapSetInstanceData iref { id_ with attrs := id_.attrs.erase name }
      return .none
    | _ => throwTypeError "object.__delattr__() takes 2 arguments"
  | _, _ => throwAttributeError s!"type '{typeName_}' has no attribute '{method}'"
where
  hexCharToNat (c : Char) : InterpM Nat := do
    if '0' ≤ c && c ≤ '9' then return c.toNat - '0'.toNat
    else if 'a' ≤ c && c ≤ 'f' then return c.toNat - 'a'.toNat + 10
    else if 'A' ≤ c && c ≤ 'F' then return c.toNat - 'A'.toNat + 10
    else throwValueError s!"non-hexadecimal number found in fromhex() arg"

-- List methods
partial def callListMethod (ref : HeapRef) (method : String) (args : List Value)
    : InterpM Value := do
  match method with
  | "append" =>
    match args with
    | [v] => do heapSetList ref ((← heapGetList ref).push v); return .none
    | _ => throwTypeError "append() takes exactly one argument"
  | "extend" =>
    match args with
    | [v] => do
      let items ← iterValues v
      heapSetList ref ((← heapGetList ref) ++ items); return .none
    | _ => throwTypeError "extend() takes exactly one argument"
  | "pop" => do
    let arr ← heapGetList ref
    match args with
    | [] =>
      if arr.isEmpty then throwIndexError "pop from empty list"
      heapSetList ref arr.pop; return arr.back!
    | [.int i] => do
      let ni := normalizeIndex i arr.size
      if ni < 0 || ni >= arr.size then throwIndexError "pop index out of range"
      let elem := arr[ni.toNat]!
      heapSetList ref (arr.toList.take ni.toNat ++ arr.toList.drop (ni.toNat + 1) |>.toArray); return elem
    | _ => throwTypeError "pop() takes at most 1 argument"
  | "insert" =>
    match args with
    | [.int i, v] => do
      let arr ← heapGetList ref
      let ni := min (max 0 (normalizeIndex i arr.size)).toNat arr.size
      let before := arr.toList.take ni
      let after := arr.toList.drop ni
      heapSetList ref (before ++ [v] ++ after).toArray; return .none
    | _ => throwTypeError "insert() takes exactly 2 arguments"
  | "remove" =>
    match args with
    | [v] => do
      let arr ← heapGetList ref
      let mut newArr : Array Value := #[]
      let mut found := false
      for elem in arr do
        if !found && (← valueEq elem v) then found := true
        else newArr := newArr.push elem
      if !found then throwValueError "list.remove(x): x not in list"
      heapSetList ref newArr; return .none
    | _ => throwTypeError "remove() takes exactly one argument"
  | "reverse" => do heapSetList ref (← heapGetList ref).reverse; return .none
  | "clear" => do heapSetList ref #[]; return .none
  | "copy" => do allocList (← heapGetList ref)
  | "index" =>
    match args with
    | [v] => do
      let arr ← heapGetList ref
      for i in [:arr.size] do
        if ← valueEq arr[i]! v then return .int i
      throwValueError "x is not in list"
    | _ => throwTypeError "index() takes 1 argument"
  | "count" =>
    match args with
    | [v] => do
      let arr ← heapGetList ref
      let mut count : Nat := 0
      for elem in arr do
        if ← valueEq elem v then count := count + 1
      return .int count
    | _ => throwTypeError "count() takes exactly one argument"
  | "sort" => do
    let arr ← heapGetList ref
    let sorted ← listInsertionSort arr.toList
    heapSetList ref sorted.toArray; return .none
  | _ => throwAttributeError s!"'list' object has no attribute '{method}'"
where
  listInsertionSort (xs : List Value) : InterpM (List Value) := do
    let mut result : List Value := []
    for x in xs do
      result ← listInsertOne x result
    return result
  listInsertOne (x : Value) (sorted : List Value) : InterpM (List Value) := do
    match sorted with
    | [] => return [x]
    | y :: ys =>
      if ← evalCmpOp .ltE x y then return x :: y :: ys
      else return y :: (← listInsertOne x ys)

-- Dict methods
partial def callDictMethod (ref : HeapRef) (method : String) (args : List Value)
    : InterpM Value := do
  match method with
  | "get" =>
    match args with
    | [key] => do
      let pairs ← heapGetDict ref
      for (k, v) in pairs do
        if ← valueEq k key then return v
      return .none
    | [key, default_] => do
      let pairs ← heapGetDict ref
      for (k, v) in pairs do
        if ← valueEq k key then return v
      return default_
    | _ => throwTypeError "get() takes 1 or 2 arguments"
  | "keys" => do allocList ((← heapGetDict ref).map Prod.fst)
  | "values" => do allocList ((← heapGetDict ref).map Prod.snd)
  | "items" => do
    let pairs ← heapGetDict ref
    allocList (pairs.map fun (k, v) => Value.tuple #[k, v])
  | "pop" =>
    match args with
    | [key] => do
      let pairs ← heapGetDict ref
      let mut result : Option Value := none
      let mut newPairs : Array (Value × Value) := #[]
      for (k, v) in pairs do
        if result.isNone && (← valueEq k key) then result := some v
        else newPairs := newPairs.push (k, v)
      match result with
      | some v => do heapSetDict ref newPairs; return v
      | none => throwKeyError (← valueRepr key)
    | [key, default_] => do
      let pairs ← heapGetDict ref
      let mut result : Option Value := none
      let mut newPairs : Array (Value × Value) := #[]
      for (k, v) in pairs do
        if result.isNone && (← valueEq k key) then result := some v
        else newPairs := newPairs.push (k, v)
      match result with
      | some v => do heapSetDict ref newPairs; return v
      | none => return default_
    | _ => throwTypeError "pop() takes 1 or 2 arguments"
  | "update" =>
    match args with
    | [.dict otherRef] => do
      let pairs ← heapGetDict ref
      let otherPairs ← heapGetDict otherRef
      let mut newPairs := pairs
      for (k, v) in otherPairs do
        let mut found := false
        for i in [:newPairs.size] do
          if ← valueEq newPairs[i]!.1 k then
            newPairs := newPairs.set! i (k, v); found := true; break
        if !found then newPairs := newPairs.push (k, v)
      heapSetDict ref newPairs; return .none
    | _ => throwTypeError "update() takes 1 argument"
  | "clear" => do heapSetDict ref #[]; return .none
  | "copy" => do allocDict (← heapGetDict ref)
  | "setdefault" =>
    match args with
    | [key] => do
      let pairs ← heapGetDict ref
      for (k, v) in pairs do
        if ← valueEq k key then return v
      let newPairs := pairs.push (key, .none)
      heapSetDict ref newPairs; return .none
    | [key, default_] => do
      let pairs ← heapGetDict ref
      for (k, v) in pairs do
        if ← valueEq k key then return v
      let newPairs := pairs.push (key, default_)
      heapSetDict ref newPairs; return default_
    | _ => throwTypeError "setdefault() takes 1 or 2 arguments"
  | _ => throwAttributeError s!"'dict' object has no attribute '{method}'"

-- String methods
partial def callStrMethod (s : String) (method : String) (args : List Value)
    : InterpM Value := do
  match method with
  | "upper" => return .str s.toUpper
  | "lower" => return .str s.toLower
  | "strip" =>
    let chars := s.toList
    let trimmed := chars.dropWhile Char.isWhitespace |>.reverse |>.dropWhile Char.isWhitespace |>.reverse
    return .str (String.ofList trimmed)
  | "split" =>
    match args with
    | [] => do
      -- Split on whitespace
      let parts := splitOnWhitespace s
      allocList (parts.map Value.str).toArray
    | [.str sep] => do
      let parts := splitOnStr s sep
      allocList (parts.map Value.str).toArray
    | _ => throwTypeError "split() takes at most 1 argument"
  | "join" =>
    match args with
    | [iter] => do
      let items ← iterValues iter
      let strs ← items.toList.mapM fun v =>
        match v with
        | .str sub => pure sub
        | _ => throwTypeError "sequence item: expected str"
      return .str (String.intercalate s strs)
    | _ => throwTypeError "join() takes exactly one argument"
  | "replace" =>
    match args with
    | [.str old, .str new_] => return .str (s.replace old new_)
    | _ => throwTypeError "replace() takes 2 arguments"
  | "startswith" =>
    match args with
    | [.str prefix_] => return .bool (s.startsWith prefix_)
    | _ => throwTypeError "startswith() takes 1 argument"
  | "endswith" =>
    match args with
    | [.str suffix_] => return .bool (s.endsWith suffix_)
    | _ => throwTypeError "endswith() takes 1 argument"
  | "find" =>
    match args with
    | [.str sub] => return .int (stringFind s sub)
    | _ => throwTypeError "find() takes 1 argument"
  | "removeprefix" =>
    match args with
    | [.str prefix_] =>
      if s.startsWith prefix_ then return .str (String.ofList (s.toList.drop prefix_.length))
      else return .str s
    | _ => throwTypeError "removeprefix() takes 1 argument"
  | "removesuffix" =>
    match args with
    | [.str suffix_] =>
      if s.endsWith suffix_ then
        return .str (String.ofList (s.toList.take (s.length - suffix_.length)))
      else return .str s
    | _ => throwTypeError "removesuffix() takes 1 argument"
  | "isdigit" => return .bool (s.all Char.isDigit && !s.isEmpty)
  | "isalpha" => return .bool (s.all Char.isAlpha && !s.isEmpty)
  | "encode" => return .bytes s.toUTF8
  | "zfill" =>
    match args with
    | [.int width] =>
      let w := width.toNat
      if s.length >= w then return .str s
      let padding := String.ofList (List.replicate (w - s.length) '0')
      return .str (padding ++ s)
    | _ => throwTypeError "zfill() takes 1 argument"
  | "format" => do
    -- Python str.format(): supports {}, {0}, {1}, {{, }}
    let chars := s.toList
    let mut result : List Char := []
    let mut i := 0
    let mut autoIdx := 0
    while i < chars.length do
      let c := chars[i]!
      if c == '{' then
        if i + 1 < chars.length && chars[i + 1]! == '{' then
          result := result ++ ['{']
          i := i + 2
        else
          -- Find closing }
          let mut j := i + 1
          while j < chars.length && chars[j]! != '}' do j := j + 1
          if j >= chars.length then
            throwValueError "Single '{' encountered in format string"
          let fieldContent := String.ofList (chars.drop (i + 1) |>.take (j - i - 1))
          -- Strip format spec after ':'
          let fieldName := match fieldContent.splitOn ":" with
            | name :: _ => name
            | [] => ""
          let argVal ←
            if fieldName.isEmpty then do
              if autoIdx >= args.length then throwIndexError "Replacement index out of range for positional args tuple"
              let v := args[autoIdx]!
              autoIdx := autoIdx + 1
              pure v
            else match fieldName.toNat? with
              | some idx =>
                if idx >= args.length then throwIndexError s!"Replacement index {idx} out of range for positional args tuple"
                pure args[idx]!
              | none => throwKeyError s!"'{fieldName}'"
          let valStr ← valueToStr argVal
          result := result ++ valStr.toList
          i := j + 1
      else if c == '}' then
        if i + 1 < chars.length && chars[i + 1]! == '}' then
          result := result ++ ['}']
          i := i + 2
        else
          throwValueError "Single '}' encountered in format string"
      else
        result := result ++ [c]
        i := i + 1
    return .str (String.ofList result)
  | "count" =>
    match args with
    | [.str sub] => return .int (stringCount s sub)
    | _ => throwTypeError "count() takes 1 argument"
  | "lstrip" =>
    let chars := s.toList.dropWhile Char.isWhitespace
    return .str (String.ofList chars)
  | "rstrip" =>
    let chars := s.toList.reverse.dropWhile Char.isWhitespace |>.reverse
    return .str (String.ofList chars)
  | _ => throwAttributeError s!"'str' object has no attribute '{method}'"
where
  stringFind (haystack sub : String) : Int :=
    let hLen := haystack.length
    let sLen := sub.length
    if sLen == 0 then 0
    else if sLen > hLen then -1
    else Id.run do
      for i in [:hLen - sLen + 1] do
        if (haystack.toList.drop i).take sLen == sub.toList then return (i : Int)
      return -1
  splitOnWhitespace (str : String) : List String :=
    let chars := str.toList
    Id.run do
      let mut result : List String := []
      let mut current : List Char := []
      for c in chars do
        if c.isWhitespace then
          if !current.isEmpty then
            result := result ++ [String.ofList current.reverse]
            current := []
        else
          current := c :: current
      if !current.isEmpty then
        result := result ++ [String.ofList current.reverse]
      return result
  matchAt (chars : List Char) (i : Nat) (pattern : List Char) : Bool :=
    pattern.length ≤ chars.length - i &&
    (chars.drop i).take pattern.length == pattern
  splitOnStr (haystack sep : String) : List String :=
    if sep.isEmpty then [haystack]
    else Id.run do
      let chars := haystack.toList
      let sepChars := sep.toList
      let mut result : List String := []
      let mut current : List Char := []
      let mut i : Nat := 0
      while i < chars.length do
        if matchAt chars i sepChars then
          result := result ++ [String.ofList current.reverse]
          current := []
          i := i + sepChars.length
        else
          current := chars[i]! :: current
          i := i + 1
      result := result ++ [String.ofList current.reverse]
      return result
  stringCount (haystack sub : String) : Nat :=
    if sub.isEmpty then haystack.length + 1
    else Id.run do
      let mut count : Nat := 0
      let mut i : Nat := 0
      let hChars := haystack.toList
      let sChars := sub.toList
      while i + sChars.length <= hChars.length do
        if matchAt hChars i sChars then
          count := count + 1
          i := i + sChars.length
        else
          i := i + 1
      return count

-- Set methods
partial def callSetMethod (ref : HeapRef) (method : String) (args : List Value)
    : InterpM Value := do
  match method with
  | "add" =>
    match args with
    | [v] => do
      let arr ← heapGetSet ref
      let mut found := false
      for elem in arr do
        if ← valueEq elem v then found := true; break
      if !found then heapSetSet ref (arr.push v)
      return .none
    | _ => throwTypeError "add() takes exactly one argument"
  | "remove" =>
    match args with
    | [v] => do
      let arr ← heapGetSet ref
      let mut newArr : Array Value := #[]
      let mut found := false
      for elem in arr do
        if !found && (← valueEq elem v) then found := true
        else newArr := newArr.push elem
      if !found then throwKeyError "element not in set"
      heapSetSet ref newArr; return .none
    | _ => throwTypeError "remove() takes exactly one argument"
  | "discard" =>
    match args with
    | [v] => do
      let arr ← heapGetSet ref
      let mut newArr : Array Value := #[]
      for elem in arr do
        if !(← valueEq elem v) then newArr := newArr.push elem
      heapSetSet ref newArr; return .none
    | _ => throwTypeError "discard() takes exactly one argument"
  | "clear" => do heapSetSet ref #[]; return .none
  | "copy" => do allocSet (← heapGetSet ref)
  | "union" =>
    match args with
    | [other] => do
      let a ← heapGetSet ref
      let b ← iterValues other
      let mut result := a
      for elem in b do
        let mut found := false
        for existing in result do
          if ← valueEq existing elem then found := true; break
        if !found then result := result.push elem
      allocSet result
    | _ => throwTypeError "union() takes exactly one argument"
  | "intersection" =>
    match args with
    | [other] => do
      let a ← heapGetSet ref
      let b ← iterValues other
      let mut result : Array Value := #[]
      for elem in a do
        let mut found := false
        for otherElem in b do
          if ← valueEq elem otherElem then found := true; break
        if found then result := result.push elem
      allocSet result
    | _ => throwTypeError "intersection() takes exactly one argument"
  | "difference" =>
    match args with
    | [other] => do
      let a ← heapGetSet ref
      let b ← iterValues other
      let mut result : Array Value := #[]
      for elem in a do
        let mut found := false
        for otherElem in b do
          if ← valueEq elem otherElem then found := true; break
        if !found then result := result.push elem
      allocSet result
    | _ => throwTypeError "difference() takes exactly one argument"
  | "symmetric_difference" =>
    match args with
    | [other] => do
      let a ← heapGetSet ref
      let b ← iterValues other
      let mut result : Array Value := #[]
      for elem in a do
        let mut found := false
        for otherElem in b do
          if ← valueEq elem otherElem then found := true; break
        if !found then result := result.push elem
      for elem in b do
        let mut found := false
        for otherElem in a do
          if ← valueEq elem otherElem then found := true; break
        if !found then result := result.push elem
      allocSet result
    | _ => throwTypeError "symmetric_difference() takes exactly one argument"
  | "issubset" =>
    match args with
    | [other] => do
      let a ← heapGetSet ref
      let b ← iterValues other
      for elem in a do
        let mut found := false
        for otherElem in b do
          if ← valueEq elem otherElem then found := true; break
        if !found then return .bool false
      return .bool true
    | _ => throwTypeError "issubset() takes exactly one argument"
  | "issuperset" =>
    match args with
    | [other] => do
      let a ← heapGetSet ref
      let b ← iterValues other
      for elem in b do
        let mut found := false
        for otherElem in a do
          if ← valueEq elem otherElem then found := true; break
        if !found then return .bool false
      return .bool true
    | _ => throwTypeError "issuperset() takes exactly one argument"
  | "isdisjoint" =>
    match args with
    | [other] => do
      let a ← heapGetSet ref
      let b ← iterValues other
      for elem in a do
        for otherElem in b do
          if ← valueEq elem otherElem then return .bool false
      return .bool true
    | _ => throwTypeError "isdisjoint() takes exactly one argument"
  | _ => throwAttributeError s!"'set' object has no attribute '{method}'"

-- ============================================================
-- @dataclass implementation
-- ============================================================

/-- Apply @dataclass decorator to a class. Generates __init__, __repr__, __eq__. -/
partial def applyDataclass (cls : Value) (frozen : Bool) (useSlots : Bool)
    : InterpM Value := do
  match cls with
  | .classObj cref => do
    let cd ← heapGetClassData cref
    -- Read __annotations__ to get field names
    let fieldNames ← match cd.ns["__annotations__"]? with
      | some (.dict annRef) => do
        let pairs ← heapGetDict annRef
        pure (pairs.map fun (k, _) =>
          match k with | .str s => s | _ => "").toList
      | _ => pure ([] : List String)
    -- Get default values from class namespace
    let mut fieldsWithDefaults : List (String × Option Value) := []
    for name in fieldNames do
      fieldsWithDefaults := fieldsWithDefaults ++ [(name, cd.ns[name]?)]
    -- Generate __init__ if not already defined
    let mut nsUpdated := cd.ns
    if cd.ns["__init__"]?.isNone then
      -- Build synthetic __init__: def __init__(self, field1, field2, ...): self.field1 = field1; ...
      -- For frozen dataclasses, use object.__setattr__(self, name, value) to bypass frozen __setattr__
      let selfArg := Arg.mk "self" none dummySpan
      let paramArgs := fieldNames.map fun n => Arg.mk n none dummySpan
      let bodyStmts := fieldNames.map fun n =>
        if frozen then
          -- object.__setattr__(self, 'name', value)
          Stmt.expr (Expr.call
            (Expr.attribute (Expr.name "object" dummySpan) "__setattr__" dummySpan)
            [Expr.name "self" dummySpan, Expr.constant (.string n) dummySpan, Expr.name n dummySpan]
            [] dummySpan) dummySpan
        else
          Stmt.assign [Expr.attribute (Expr.name "self" dummySpan) n dummySpan]
            (Expr.name n dummySpan) dummySpan
      -- Separate defaults: only trailing fields with defaults
      let defaults := fieldsWithDefaults.filterMap fun (_, d) => d
      let args := Arguments.mk [] (selfArg :: paramArgs) none [] [] none
        (defaults.map fun v => Expr.constant (valueToConstant v) dummySpan)
      let fd := FuncData.mk "__init__" args bodyStmts defaults.toArray #[] #[] false
      let initVal ← allocFunc fd
      nsUpdated := nsUpdated.insert "__init__" initVal
    -- Generate __repr__ if not already defined
    if cd.ns["__repr__"]?.isNone then
      -- Build synthetic __repr__: returns "ClassName(f1=v1, f2=v2)"
      -- We build it as: return ClassName + "(" + repr(self.f1) + ", " + ... + ")"
      let selfArg := Arg.mk "self" none dummySpan
      let args := Arguments.mk [] [selfArg] none [] [] none []
      -- Build f-string-like expression using string concatenation
      let mut reprExpr : Expr := Expr.constant (.string s!"{cd.name}(") dummySpan
      let mut fieldIdx : Nat := 0
      for name in fieldNames do
        let pfx := if fieldIdx > 0
          then Expr.constant (.string s!", {name}=") dummySpan
          else Expr.constant (.string s!"{name}=") dummySpan
        let fieldAccess := Expr.attribute (Expr.name "self" dummySpan) name dummySpan
        let reprCall := Expr.call (Expr.name "repr" dummySpan) [fieldAccess] [] dummySpan
        reprExpr := Expr.binOp (Expr.binOp reprExpr .add pfx dummySpan) .add reprCall dummySpan
        fieldIdx := fieldIdx + 1
      reprExpr := Expr.binOp reprExpr .add (Expr.constant (.string ")") dummySpan) dummySpan
      let bodyStmts := [Stmt.return_ (some reprExpr) dummySpan]
      let fd := FuncData.mk "__repr__" args bodyStmts #[] #[] #[] false
      let reprVal ← allocFunc fd
      nsUpdated := nsUpdated.insert "__repr__" reprVal
    -- Generate __eq__ if not already defined
    if cd.ns["__eq__"]?.isNone then
      -- Build synthetic __eq__: compare all fields
      -- return self.f1 == other.f1 and self.f2 == other.f2 and ...
      let selfArg := Arg.mk "self" none dummySpan
      let otherArg := Arg.mk "other" none dummySpan
      let args := Arguments.mk [] [selfArg, otherArg] none [] [] none []
      let bodyExpr := if fieldNames.isEmpty then
        Expr.constant .true_ dummySpan
      else
        let comparisons := fieldNames.map fun name =>
          Expr.compare
            (Expr.attribute (Expr.name "self" dummySpan) name dummySpan)
            [(.eq, Expr.attribute (Expr.name "other" dummySpan) name dummySpan)]
            dummySpan
        match comparisons with
        | [single] => single
        | _ => Expr.boolOp .and_ comparisons dummySpan
      let bodyStmts := [Stmt.return_ (some bodyExpr) dummySpan]
      let fd := FuncData.mk "__eq__" args bodyStmts #[] #[] #[] false
      let eqVal ← allocFunc fd
      nsUpdated := nsUpdated.insert "__eq__" eqVal
    -- Apply frozen: add __setattr__ and __delattr__ that raise
    if frozen then
      -- __setattr__ that raises FrozenInstanceError
      let selfArg := Arg.mk "self" none dummySpan
      let nameArg := Arg.mk "name" none dummySpan
      let valueArg := Arg.mk "value" none dummySpan
      let frozenSetArgs := Arguments.mk [] [selfArg, nameArg, valueArg] none [] [] none []
      let frozenSetBody := [Stmt.raise_
        (some (Expr.call (Expr.name "AttributeError" dummySpan)
          [Expr.constant (.string "cannot assign to field") dummySpan] [] dummySpan))
        none dummySpan]
      let frozenSetFd := FuncData.mk "__setattr__" frozenSetArgs frozenSetBody #[] #[] #[] false
      let frozenSetVal ← allocFunc frozenSetFd
      nsUpdated := nsUpdated.insert "__setattr__" frozenSetVal
      -- __delattr__ that raises
      let delArgs := Arguments.mk [] [selfArg, nameArg] none [] [] none []
      let frozenDelBody := [Stmt.raise_
        (some (Expr.call (Expr.name "AttributeError" dummySpan)
          [Expr.constant (.string "cannot delete field") dummySpan] [] dummySpan))
        none dummySpan]
      let frozenDelFd := FuncData.mk "__delattr__" delArgs frozenDelBody #[] #[] #[] false
      let frozenDelVal ← allocFunc frozenDelFd
      nsUpdated := nsUpdated.insert "__delattr__" frozenDelVal
    -- Apply slots
    let slotsOpt := if useSlots then some fieldNames.toArray else cd.slots
    -- Update class data
    heapSetClassData cref { cd with ns := nsUpdated, slots := slotsOpt }
    return cls
  | _ => throwTypeError "dataclass() takes a class"

-- ============================================================
-- Pydantic BaseModel processing
-- ============================================================

/-- Apply Pydantic model processing to a class that inherits from BaseModel.
    Generates __init__, __repr__, __eq__, __hash__, model_fields, and frozen support. -/
partial def applyPydanticModelProcessing (_classVal : Value) (cref : HeapRef)
    (_bases : Array Value) : InterpM Unit := do
  let cd ← heapGetClassData cref
  -- 1. Collect inherited fields from parent BaseModel classes (MRO order)
  let mut inheritedFields : List (String × Option Value) := []
  let mut inheritedAnnPairs : Array (Value × Value) := #[]
  for mroEntry in cd.mro do
    match mroEntry with
    | .classObj mref => do
      if mref == cref then continue  -- skip self
      let mcd ← heapGetClassData mref
      if mcd.name == "BaseModel" then continue  -- skip BaseModel itself
      if mcd.ns["__pydantic_model__"]? != some (.bool true) then continue
      -- Get field names from parent's __pydantic_field_names__
      match mcd.ns["__pydantic_field_names__"]? with
      | some (.list fnRef) => do
        let fnArr ← heapGetList fnRef
        for fv in fnArr do
          match fv with
          | .str fname =>
            if !(inheritedFields.any fun (n, _) => n == fname) then
              inheritedFields := inheritedFields ++ [(fname, mcd.ns[fname]?)]
              inheritedAnnPairs := inheritedAnnPairs.push (.str fname, .str "Any")
          | _ => pure ()
      | _ => pure ()
    | _ => pure ()
  -- 2. Read own __annotations__
  let ownFieldNames ← match cd.ns["__annotations__"]? with
    | some (.dict annRef) => do
      let pairs ← heapGetDict annRef
      pure (pairs.map fun (k, _) =>
        match k with | .str s => s | _ => "").toList
    | _ => pure ([] : List String)
  -- 3. Merge: inherited fields first, then own fields (own overrides inherited)
  let mut allFields : List (String × Option Value) := []
  for (name, defVal) in inheritedFields do
    if !(ownFieldNames.contains name) then
      allFields := allFields ++ [(name, defVal)]
  for name in ownFieldNames do
    allFields := allFields ++ [(name, cd.ns[name]?)]
  let allFieldNames := allFields.map Prod.fst
  -- 4. Parse model_config (walk MRO to find it if not in own namespace)
  let modelConfigDict ← do
    match cd.ns["model_config"]? with
    | some (.dict configRef) => pure (some configRef)
    | _ => do
      let mut found : Option HeapRef := none
      for mroEntry in cd.mro do
        if found.isSome then break
        match mroEntry with
        | .classObj mref => do
          if mref == cref then continue
          let mcd ← heapGetClassData mref
          match mcd.ns["model_config"]? with
          | some (.dict ref) => found := some ref
          | _ => pure ()
        | _ => pure ()
      pure found
  let configAndAlias : PydanticConfig × Option Value ← match modelConfigDict with
    | some configRef => do
      let pairs ← heapGetDict configRef
      let cfg := parsePydanticConfigFromPairs pairs
      -- Extract alias_generator (a callable, not stored in PydanticConfig)
      let agFn := (pairs.toList.find? fun (k, _) =>
        Value.beq k (.str "alias_generator")).map Prod.snd
      pure (cfg, agFn)
    | none => pure (default, none)
  let config := configAndAlias.1
  let aliasGenFn := configAndAlias.2
  -- 4a-inherit. Inherit alias_generator from parent if not set
  let aliasGen ← match aliasGenFn with
    | some fn => pure (some fn)
    | none => do
      let mut inherited : Option Value := none
      for mroEntry in cd.mro do
        if inherited.isSome then break
        match mroEntry with
        | .classObj mref => do
          if mref == cref then continue
          let mcd ← heapGetClassData mref
          if mcd.ns["__pydantic_model__"]? != some (.bool true) then continue
          match mcd.ns["__pydantic_alias_generator__"]? with
          | some fn => inherited := some fn
          | none => pure ()
        | _ => pure ()
      pure inherited
  -- 4b. Scan namespace for validator/serializer marker tuples
  let mut fieldValidators : Array (String × String × Value) := #[]
  let mut modelValidatorsBefore : Array Value := #[]
  let mut modelValidatorsAfter : Array Value := #[]
  let mut fieldSerializers : Array (String × String × Value) := #[]
  let mut modelSerializers : Array (String × String × Value) := #[]
  let mut cleanedNs := cd.ns
  for (nsName, val) in cd.ns do
    let marker := match val with
      | .tuple elems =>
        match elems.toList with
        | Value.str tag :: _ => tag
        | _ => ""
      | _ => ""
    if marker == "__pydantic_field_validator__" then do
      match val with
      | .tuple elems => do
        let mode := match elems.toList with | _ :: Value.str m :: _ => m | _ => "after"
        let func := if elems.size > 3 then elems[3]! else Value.none
        if elems.size > 2 then
          match elems[2]! with
          | .list fnRef => do
            let arr ← heapGetList fnRef
            let names := arr.filterMap fun | .str s => some s | _ => none
            fieldValidators := fieldValidators ++ (names.map fun fname => (fname, mode, func))
          | _ => pure ()
        cleanedNs := cleanedNs.insert nsName func
      | _ => pure ()
    else if marker == "__pydantic_model_validator__" then do
      match val with
      | .tuple elems => do
        let mode := match elems.toList with | _ :: Value.str m :: _ => m | _ => "after"
        let func := if elems.size > 2 then elems[2]! else Value.none
        if mode == "before" then
          modelValidatorsBefore := modelValidatorsBefore.push func
        else
          modelValidatorsAfter := modelValidatorsAfter.push func
        cleanedNs := cleanedNs.insert nsName func
      | _ => pure ()
    else if marker == "__pydantic_field_serializer__" then do
      match val with
      | .tuple elems => do
        let func := if elems.size > 3 then elems[3]! else Value.none
        let whenUsed := match elems.toList with | _ :: Value.str w :: _ => w | _ => "always"
        if elems.size > 2 then
          match elems[2]! with
          | .list fnRef => do
            let arr ← heapGetList fnRef
            let names := arr.filterMap fun | .str s => some s | _ => none
            fieldSerializers := fieldSerializers ++ (names.map fun fname => (fname, whenUsed, func))
          | _ => pure ()
        cleanedNs := cleanedNs.insert nsName func
      | _ => pure ()
    else if marker == "__pydantic_model_serializer__" then do
      match val with
      | .tuple elems => do
        let mode := match elems.toList with | _ :: Value.str m :: _ => m | _ => "plain"
        let whenUsed := match elems.toList with | _ :: _ :: Value.str w :: _ => w | _ => "always"
        let func := if elems.size > 3 then elems[3]! else Value.none
        modelSerializers := modelSerializers.push (mode, whenUsed, func)
        cleanedNs := cleanedNs.insert nsName func
      | _ => pure ()
    else pure ()
  -- 4c. Inherit validators from parent BaseModel classes
  for mroEntry in cd.mro do
    match mroEntry with
    | .classObj mref => do
      if mref == cref then continue
      let mcd ← heapGetClassData mref
      if mcd.ns["__pydantic_model__"]? != some (.bool true) then continue
      -- Inherit field validators
      match mcd.ns["__pydantic_field_validators__"]? with
      | some (.list fvRef) => do
        let fvArr ← heapGetList fvRef
        for fv in fvArr do
          match fv with
          | .tuple elems =>
            if elems.size == 3 then
              match elems[0]!, elems[1]! with
              | .str fname, .str mode =>
                if !(fieldValidators.any fun (f, _, _) => f == fname) then
                  fieldValidators := fieldValidators.push (fname, mode, elems[2]!)
              | _, _ => pure ()
          | _ => pure ()
      | _ => pure ()
      -- Inherit model validators (before)
      match mcd.ns["__pydantic_model_validators_before__"]? with
      | some (.list mvRef) => do
        let mvArr ← heapGetList mvRef
        for mv in mvArr do
          modelValidatorsBefore := modelValidatorsBefore.push mv
      | _ => pure ()
      -- Inherit model validators (after)
      match mcd.ns["__pydantic_model_validators_after__"]? with
      | some (.list mvRef) => do
        let mvArr ← heapGetList mvRef
        for mv in mvArr do
          modelValidatorsAfter := modelValidatorsAfter.push mv
      | _ => pure ()
      -- Inherit field serializers
      match mcd.ns["__pydantic_field_serializers__"]? with
      | some (.list fsRef) => do
        let fsArr ← heapGetList fsRef
        for fs in fsArr do
          match fs with
          | .tuple elems =>
            if elems.size == 3 then
              match elems[0]!, elems[1]! with
              | .str fname, .str whenUsed =>
                if !(fieldSerializers.any fun (f, _, _) => f == fname) then
                  fieldSerializers := fieldSerializers.push (fname, whenUsed, elems[2]!)
              | _, _ => pure ()
          | _ => pure ()
      | _ => pure ()
    | _ => pure ()
  -- 4d. Store collected validators/serializers as lists on the class
  let fvStored := fieldValidators.map fun (f, m, func) =>
    Value.tuple #[.str f, .str m, func]
  let fvRef ← heapAlloc (.listObj fvStored)
  let mvbRef ← heapAlloc (.listObj modelValidatorsBefore)
  let mvaRef ← heapAlloc (.listObj modelValidatorsAfter)
  let fsStored := fieldSerializers.map fun (f, w, func) =>
    Value.tuple #[.str f, .str w, func]
  let fsRef ← heapAlloc (.listObj fsStored)
  let msStored := modelSerializers.map fun (m, w, func) =>
    Value.tuple #[.str m, .str w, func]
  let msRef ← heapAlloc (.listObj msStored)
  -- 5. Build namespace updates
  let mut nsUpdated := cleanedNs
  -- Mark as Pydantic model
  nsUpdated := nsUpdated.insert "__pydantic_model__" (.bool true)
  -- Store field names list for inheritance and model_copy/model_dump
  let fnArr := allFieldNames.map (fun n => Value.str n) |>.toArray
  let fnRef ← heapAlloc (.listObj fnArr)
  nsUpdated := nsUpdated.insert "__pydantic_field_names__" (.list fnRef)
  -- Store config
  nsUpdated := nsUpdated.insert "__pydantic_frozen__" (.bool config.frozen)
  nsUpdated := nsUpdated.insert "__pydantic_extra__" (.str config.extra)
  nsUpdated := nsUpdated.insert "__pydantic_populate_by_name__" (.bool config.populateByName)
  -- Store alias_generator if present
  match aliasGen with
  | some fn => nsUpdated := nsUpdated.insert "__pydantic_alias_generator__" fn
  | none => pure ()
  -- Store validators/serializers
  nsUpdated := nsUpdated.insert "__pydantic_field_validators__" (.list fvRef)
  nsUpdated := nsUpdated.insert "__pydantic_model_validators_before__" (.list mvbRef)
  nsUpdated := nsUpdated.insert "__pydantic_model_validators_after__" (.list mvaRef)
  nsUpdated := nsUpdated.insert "__pydantic_field_serializers__" (.list fsRef)
  nsUpdated := nsUpdated.insert "__pydantic_model_serializers__" (.list msRef)
  -- 6. Build model_fields dict: {field_name: {"annotation": ..., "default": ...}}
  let mut mfPairs : Array (Value × Value) := #[]
  for (name, defVal) in allFields do
    let mut infoPairs : Array (Value × Value) := #[]
    infoPairs := infoPairs.push (.str "annotation", .str "Any")
    match defVal with
    | some v => infoPairs := infoPairs.push (.str "default", v)
    | none => pure ()
    let infoDict ← allocDict infoPairs
    mfPairs := mfPairs.push (.str name, infoDict)
  let mfDict ← allocDict mfPairs
  nsUpdated := nsUpdated.insert "model_fields" mfDict
  -- 6b. Build __get_pydantic_core_schema__ field schema map
  -- For each field, check if the annotation type has __get_pydantic_core_schema__
  let rawAnns ← match cd.ns["__annotations_raw__"]? with
    | some (.dict ref) => heapGetDict ref
    | _ => pure #[]
  let mut fieldSchemaPairs : Array (Value × Value) := #[]
  for (nameV, typeV) in rawAnns do
    match nameV with
    | .str fname =>
      -- Check if the type has __get_pydantic_core_schema__ (walk MRO)
      let schemaOpt ← match typeV with
        | .classObj tref => do
          let tcd ← heapGetClassData tref
          -- Walk MRO to find __get_pydantic_core_schema__
          let mut hookFound : Option Value := none
          for mroEntry in tcd.mro do
            if hookFound.isSome then break
            match mroEntry with
            | .classObj mref => do
              let mcd ← heapGetClassData mref
              if let some fn := mcd.ns["__get_pydantic_core_schema__"]? then
                hookFound := some fn
            | _ => pure ()
          -- Also check own namespace (for classes without full MRO)
          if hookFound.isNone then
            hookFound := tcd.ns["__get_pydantic_core_schema__"]?
          match hookFound with
          | some hookFn => do
            -- Unwrap classmethod to get raw function, then call with (cls, source_type, handler)
            let rawFn := match hookFn with
              | .classMethod inner => inner
              | other => other
            let result ← try
              let schema ← callValueDispatch rawFn [typeV, typeV, .none] []
              pure (some schema)
            catch _ => pure none
            pure result
          | none => pure none
        | _ => pure none
      match schemaOpt with
      | some schema => fieldSchemaPairs := fieldSchemaPairs.push (.str fname, schema)
      | none => pure ()
    | _ => pure ()
  if !fieldSchemaPairs.isEmpty then do
    let fsDict ← allocDict fieldSchemaPairs
    nsUpdated := nsUpdated.insert "__pydantic_field_schemas__" fsDict
  -- 7. Merge own annotations with inherited for full __annotations__ dict
  let mut fullAnnPairs := inheritedAnnPairs
  match cd.ns["__annotations__"]? with
  | some (.dict annRef) => do
    let pairs ← heapGetDict annRef
    for p in pairs do fullAnnPairs := fullAnnPairs.push p
  | _ => pure ()
  let fullAnnRef ← heapAlloc (.dictObj fullAnnPairs)
  nsUpdated := nsUpdated.insert "__annotations__" (.dict fullAnnRef)
  -- 8. Generate __init__ if not already defined
  if cd.ns["__init__"]?.isNone then
    let selfArg := Arg.mk "self" none dummySpan
    let paramArgs := allFieldNames.map fun n => Arg.mk n none dummySpan
    -- Always use object.__setattr__ for Pydantic (compatible with frozen)
    let bodyStmts := allFieldNames.map fun n =>
      Stmt.expr (Expr.call
        (Expr.attribute (Expr.name "object" dummySpan) "__setattr__" dummySpan)
        [Expr.name "self" dummySpan, Expr.constant (.string n) dummySpan, Expr.name n dummySpan]
        [] dummySpan) dummySpan
    let fieldsWithDefaults := allFields.filterMap fun (_, d) => d
    let args := Arguments.mk [] (selfArg :: paramArgs) none [] [] none
      (fieldsWithDefaults.map fun v => Expr.constant (valueToConstant v) dummySpan)
    let fd := FuncData.mk "__init__" args bodyStmts fieldsWithDefaults.toArray #[] #[] false
    let initVal ← allocFunc fd
    nsUpdated := nsUpdated.insert "__init__" initVal
  -- 9. Generate __repr__ if not already defined
  if cd.ns["__repr__"]?.isNone then
    let selfArg := Arg.mk "self" none dummySpan
    let args := Arguments.mk [] [selfArg] none [] [] none []
    let mut reprExpr : Expr := Expr.constant (.string s!"{cd.name}(") dummySpan
    let mut fieldIdx : Nat := 0
    for name in allFieldNames do
      let pfx := if fieldIdx > 0
        then Expr.constant (.string s!", {name}=") dummySpan
        else Expr.constant (.string s!"{name}=") dummySpan
      let fieldAccess := Expr.attribute (Expr.name "self" dummySpan) name dummySpan
      let reprCall := Expr.call (Expr.name "repr" dummySpan) [fieldAccess] [] dummySpan
      reprExpr := Expr.binOp (Expr.binOp reprExpr .add pfx dummySpan) .add reprCall dummySpan
      fieldIdx := fieldIdx + 1
    reprExpr := Expr.binOp reprExpr .add (Expr.constant (.string ")") dummySpan) dummySpan
    let bodyStmts := [Stmt.return_ (some reprExpr) dummySpan]
    let fd := FuncData.mk "__repr__" args bodyStmts #[] #[] #[] false
    let reprVal ← allocFunc fd
    nsUpdated := nsUpdated.insert "__repr__" reprVal
  -- 10. Generate __eq__ if not already defined
  if cd.ns["__eq__"]?.isNone then
    let selfArg := Arg.mk "self" none dummySpan
    let otherArg := Arg.mk "other" none dummySpan
    let args := Arguments.mk [] [selfArg, otherArg] none [] [] none []
    let bodyExpr := if allFieldNames.isEmpty then
      Expr.constant .true_ dummySpan
    else
      let comparisons := allFieldNames.map fun name =>
        Expr.compare
          (Expr.attribute (Expr.name "self" dummySpan) name dummySpan)
          [(.eq, Expr.attribute (Expr.name "other" dummySpan) name dummySpan)]
          dummySpan
      match comparisons with
      | [single] => single
      | _ => Expr.boolOp .and_ comparisons dummySpan
    let bodyStmts := [Stmt.return_ (some bodyExpr) dummySpan]
    let fd := FuncData.mk "__eq__" args bodyStmts #[] #[] #[] false
    let eqVal ← allocFunc fd
    nsUpdated := nsUpdated.insert "__eq__" eqVal
  -- 11. Generate __hash__ for frozen models
  if config.frozen && cd.ns["__hash__"]?.isNone then
    -- __hash__: return hash(tuple(self.f1, self.f2, ...))
    let selfArg := Arg.mk "self" none dummySpan
    let args := Arguments.mk [] [selfArg] none [] [] none []
    let fieldExprs := allFieldNames.map fun name =>
      Expr.attribute (Expr.name "self" dummySpan) name dummySpan
    let tupleExpr := Expr.tuple fieldExprs dummySpan
    let hashExpr := Expr.call (Expr.name "hash" dummySpan) [tupleExpr] [] dummySpan
    let bodyStmts := [Stmt.return_ (some hashExpr) dummySpan]
    let fd := FuncData.mk "__hash__" args bodyStmts #[] #[] #[] false
    let hashVal ← allocFunc fd
    nsUpdated := nsUpdated.insert "__hash__" hashVal
  -- 12. Apply frozen: add __setattr__ and __delattr__ that raise
  if config.frozen then
    let selfArg := Arg.mk "self" none dummySpan
    let nameArg := Arg.mk "name" none dummySpan
    let valueArg := Arg.mk "value" none dummySpan
    let frozenSetArgs := Arguments.mk [] [selfArg, nameArg, valueArg] none [] [] none []
    let frozenSetBody := [Stmt.raise_
      (some (Expr.call (Expr.name "AttributeError" dummySpan)
        [Expr.constant (.string "Instance is frozen") dummySpan] [] dummySpan))
      none dummySpan]
    let frozenSetFd := FuncData.mk "__setattr__" frozenSetArgs frozenSetBody #[] #[] #[] false
    let frozenSetVal ← allocFunc frozenSetFd
    nsUpdated := nsUpdated.insert "__setattr__" frozenSetVal
    let delArgs := Arguments.mk [] [selfArg, nameArg] none [] [] none []
    let frozenDelBody := [Stmt.raise_
      (some (Expr.call (Expr.name "AttributeError" dummySpan)
        [Expr.constant (.string "Instance is frozen") dummySpan] [] dummySpan))
      none dummySpan]
    let frozenDelFd := FuncData.mk "__delattr__" delArgs frozenDelBody #[] #[] #[] false
    let frozenDelVal ← allocFunc frozenDelFd
    nsUpdated := nsUpdated.insert "__delattr__" frozenDelVal
  -- 13. Update the class data
  heapSetClassData cref { cd with ns := nsUpdated }

-- ============================================================
-- Pydantic model_copy: create a new instance with updated fields
-- ============================================================

/-- model_copy(update={...}) — immutable copy with field updates. -/
partial def callPydanticModelCopy (_receiver : Value) (iref : HeapRef) (cref : HeapRef)
    (args : List Value) (kwargs : List (String × Value)) : InterpM Value := do
  let id_ ← heapGetInstanceData iref
  let cd ← heapGetClassData cref
  -- Get field names
  let fieldNames ← match cd.ns["__pydantic_field_names__"]? with
    | some (.list fnRef) => do
      let fnArr ← heapGetList fnRef
      pure (fnArr.toList.filterMap fun v => match v with | .str s => some s | _ => none)
    | _ => pure ([] : List String)
  -- Parse update dict: check kwargs first (update=...), then positional args
  let updateDict ← do
    -- Check keyword arg "update"
    let kw := kwargs.find? fun (k, _) => k == "update"
    match kw with
    | some (_, .dict dref) => do
      let pairs ← heapGetDict dref
      pure (pairs.toList.filterMap fun (k, v) =>
        match k with | .str s => some (s, v) | _ => none)
    | some _ => throwTypeError "update must be a dict"
    | none => match args with
      | [] => pure ([] : List (String × Value))
      | [.dict dref] => do
        let pairs ← heapGetDict dref
        pure (pairs.toList.filterMap fun (k, v) =>
          match k with | .str s => some (s, v) | _ => none)
      | _ => throwTypeError "model_copy() takes 0 or 1 positional argument (update dict)"
  -- Build new attrs: copy all fields, then apply updates
  let mut newAttrs : Std.HashMap String Value := {}
  for name in fieldNames do
    match updateDict.find? (fun (k, _) => k == name) with
    | some (_, v) => newAttrs := newAttrs.insert name v
    | none =>
      match id_.attrs[name]? with
      | some v => newAttrs := newAttrs.insert name v
      | none => pure ()
  -- Copy non-field attrs too
  for (k, v) in id_.attrs do
    if newAttrs[k]?.isNone then
      newAttrs := newAttrs.insert k v
  -- Allocate new instance directly (bypassing __init__ and frozen __setattr__)
  allocInstance { cls := id_.cls, attrs := newAttrs }

-- ============================================================
-- Pydantic core_schema evaluation engine
-- ============================================================

/-- Evaluate a core_schema descriptor against an input value, returning the validated value.
    Schema descriptors are dicts with a "type" key indicating the validation strategy. -/
partial def evalCoreSchema (schema : Value) (input : Value) : InterpM Value := do
  match schema with
  | .dict ref => do
    let pairs ← heapGetDict ref
    let schemaType := (pairs.toList.find? fun (k, _) => Value.beq k (.str "type")).map Prod.snd
    match schemaType with
    | some (.str "union") =>
      -- Try each choice; return first success
      let choices := (pairs.toList.find? fun (k, _) => Value.beq k (.str "choices")).map Prod.snd
      match choices with
      | some (.list cRef) => do
        let cArr ← heapGetList cRef
        let mut lastErr : Option String := none
        for choice in cArr do
          let result ← try
            let v ← evalCoreSchema choice input
            pure (some v)
          catch e =>
            lastErr := some (toString e)
            pure none
          if let some v := result then return v
        throwValueError s!"No matching schema in union: {lastErr.getD "unknown"}"
      | _ => return input
    | some (.str "is-instance") =>
      -- Check isinstance; pass through if match, else fail
      let cls := (pairs.toList.find? fun (k, _) => Value.beq k (.str "cls")).map Prod.snd
      match cls with
      | some clsVal =>
        let result ← builtinIsinstance [input, clsVal]
        match result with
        | .bool true => return input
        | _ => throwTypeError s!"Expected instance of type, got {typeName input}"
      | none => return input
    | some (.str "chain") =>
      -- Apply schemas sequentially
      let steps := (pairs.toList.find? fun (k, _) => Value.beq k (.str "steps")).map Prod.snd
      match steps with
      | some (.list sRef) => do
        let sArr ← heapGetList sRef
        let mut current := input
        for step in sArr do
          current ← evalCoreSchema step current
        return current
      | _ => return input
    | some (.str "int") =>
      -- Validate integer with optional constraints
      match input with
      | .int n => do
        let ge := (pairs.toList.find? fun (k, _) => Value.beq k (.str "ge")).map Prod.snd
        let lt := (pairs.toList.find? fun (k, _) => Value.beq k (.str "lt")).map Prod.snd
        match ge with
        | some (.int lower) =>
          if n < lower then throwValueError s!"Value {n} < minimum {lower}"
        | _ => pure ()
        match lt with
        | some (.int upper) =>
          if n >= upper then throwValueError s!"Value {n} >= maximum {upper}"
        | _ => pure ()
        return input
      | _ => throwTypeError s!"Expected int, got {typeName input}"
    | some (.str "bool") =>
      match input with
      | .bool _ => return input
      | _ => throwTypeError s!"Expected bool, got {typeName input}"
    | some (.str "bytes") =>
      match input with
      | .bytes b => do
        let minLen := (pairs.toList.find? fun (k, _) => Value.beq k (.str "min_length")).map Prod.snd
        let maxLen := (pairs.toList.find? fun (k, _) => Value.beq k (.str "max_length")).map Prod.snd
        match minLen with
        | some (.int ml) =>
          if (b.size : Int) < ml then throwValueError s!"Bytes too short: {b.size} < {ml}"
        | _ => pure ()
        match maxLen with
        | some (.int ml) =>
          if (b.size : Int) > ml then throwValueError s!"Bytes too long: {b.size} > {ml}"
        | _ => pure ()
        return input
      | _ => throwTypeError s!"Expected bytes, got {typeName input}"
    | some (.str "no-info-plain-validator") =>
      -- Call the validator function with input
      let func := (pairs.toList.find? fun (k, _) => Value.beq k (.str "function")).map Prod.snd
      match func with
      | some f => callValueDispatch f [input] []
      | none => return input
    | some (.str "plain-serializer") =>
      -- Serializer schemas don't validate; pass through
      return input
    | _ => return input
  | _ => return input

-- ============================================================
-- Pydantic model_dump: serialize instance to dict
-- ============================================================

/-- model_dump(mode="python") — serialize to dict. -/
partial def callPydanticModelDump (iref : HeapRef) (cref : HeapRef)
    (_args : List Value) (kwargs : List (String × Value)) : InterpM Value := do
  let id_ ← heapGetInstanceData iref
  let cd ← heapGetClassData cref
  let mode := match kwargs.find? (fun (k, _) => k == "mode") with
    | some (_, .str m) => m | _ => "python"
  let byAlias := match kwargs.find? (fun (k, _) => k == "by_alias") with
    | some (_, .bool b) => b | _ => false
  -- Get field names
  let fieldNames ← match cd.ns["__pydantic_field_names__"]? with
    | some (.list fnRef) => do
      let fnArr ← heapGetList fnRef
      pure (fnArr.toList.filterMap fun v => match v with | .str s => some s | _ => none)
    | _ => pure ([] : List String)
  -- Build alias map if by_alias=True and alias_generator is present
  let mut aliasMap : List (String × String) := []
  if byAlias then
    match cd.ns["__pydantic_alias_generator__"]? with
    | some agFn => do
      for fname in fieldNames do
        let aliasVal ← callValueDispatch agFn [.str fname] []
        match aliasVal with
        | .str aliasName => aliasMap := aliasMap ++ [(fname, aliasName)]
        | _ => aliasMap := aliasMap ++ [(fname, fname)]
    | none => pure ()
  -- Collect field serializers for json mode
  let mut serializerMap : List (String × Value) := []
  if mode == "json" then
    match cd.ns["__pydantic_field_serializers__"]? with
    | some (.list fsRef) => do
      let fsArr ← heapGetList fsRef
      for fs in fsArr do
        match fs with
        | .tuple elems =>
          if elems.size >= 3 then
            match elems[0]! with
            | .str fname => serializerMap := serializerMap ++ [(fname, elems[2]!)]
            | _ => pure ()
        | _ => pure ()
    | _ => pure ()
  -- Build dict from field values, applying serializers if applicable
  let mut pairs : Array (Value × Value) := #[]
  let inst := Value.instance iref
  for name in fieldNames do
    match id_.attrs[name]? with
    | some v => do
      let serialized ← match serializerMap.find? (fun (f, _) => f == name) with
        | some (_, serFn) =>
          -- Call serializer: serializer(self, value, _info)
          callValueDispatch serFn [inst, v, .none] []
        | none => pure v
      -- Use alias if by_alias=True
      let key := match aliasMap.find? (fun (f, _) => f == name) with
        | some (_, aliasName) => aliasName
        | none => name
      pairs := pairs.push (.str key, serialized)
    | none => pure ()
  -- Check for model_serializer
  if mode == "json" then
    match cd.ns["__pydantic_model_serializers__"]? with
    | some (.list msRef) => do
      let msArr ← heapGetList msRef
      if msArr.size > 0 then
        match msArr[0]! with
        | .tuple elems =>
          if elems.size >= 3 then
            return ← callValueDispatch elems[2]! [inst] []
          else allocDict pairs
        | _ => allocDict pairs
      else allocDict pairs
    | _ => allocDict pairs
  else
    allocDict pairs

end

end LeanPython.Interpreter.Eval
