import LeanPython.Runtime.Builtins
import LeanPython.Parser.Core
import Std.Data.HashMap
import Std.Data.HashSet

set_option autoImplicit false

namespace LeanPython.Interpreter.Eval

open LeanPython.AST
open LeanPython.Runtime
open LeanPython.Runtime.Ops
open LeanPython.Runtime.Builtins
open LeanPython.Interpreter
open LeanPython.Parser (parse)
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
        | none => callBuiltin name args kwargs
      | _ => callBuiltin name args kwargs
    | "repr" => match args with
      | [inst@(.instance _)] =>
        match ← callDunder inst "__repr__" [] with
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
          | none => args' := args' ++ [arg]
        | other => args' := args' ++ [other]
      callBuiltin name args' kwargs
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
      else
        callBuiltin name args kwargs
  | .function ref => do
    let fd ← heapGetFunc ref
    callUserFunc fd args kwargs
  | .boundMethod receiver method => callBoundMethod receiver method args
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
            let scope ← bindFuncParams fd.params (inst :: args) kwargs fd.defaults fd.kwDefaults
            let scopeWithClass := scope.insert "__class__" definingCls
            let _ ← callRegularFunc fd scopeWithClass
          | _ => let _ ← callValueDispatch fn (inst :: args) kwargs
        | none =>
          if !args.isEmpty && newFound.isNone then
            throwTypeError s!"{cd.name}() takes no arguments"
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
      let annStr ← valueToStr (← evalExpr ann)
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
    : InterpM Value := do
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
    match id_.cls with
    | .classObj cref => do
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

end

end LeanPython.Interpreter.Eval
