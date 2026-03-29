import LeanPython.Runtime.Builtins
import Std.Data.HashMap
import Std.Data.HashSet

set_option autoImplicit false

namespace LeanPython.Interpreter.Eval

open LeanPython.AST
open LeanPython.Runtime
open LeanPython.Runtime.Ops
open LeanPython.Runtime.Builtins
open LeanPython.Interpreter
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
-- Mutual block: evalExpr, execStmt, and helpers
-- ============================================================

mutual

partial def evalExpr (e : Expr) : InterpM Value := do
  match e with
  | .name n _ => lookupVariable n
  | .constant c _ => return (constToValue c)

  | .binOp left op right _ => do
    let l ← evalExpr left
    let r ← evalExpr right
    evalBinOp op l r

  | .unaryOp op operand _ => do
    let v ← evalExpr operand
    evalUnaryOp op v

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
      let result ← evalCmpOp op leftVal rightVal
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
    allocFunc (FuncData.mk "<lambda>" params [.return_ (some body) dummySpan] defaults.toArray kwDefaults.toArray st.localScopes.toArray)

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
    allocList results

  | .await_ _ _ => throwNotImplemented "await"
  | .yield_ _ _ => throwNotImplemented "yield"
  | .yieldFrom _ _ => throwNotImplemented "yield from"

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
    -- Handle map/filter specially since they need callValueDispatch
    match name with
    | "map" => builtinMap args
    | "filter" => builtinFilter args
    | "hasattr" => builtinHasattr args
    | "getattr" => builtinGetattr args
    | _ => callBuiltin name args kwargs
  | .function ref => do
    let fd ← heapGetFunc ref
    callUserFunc fd args kwargs
  | .boundMethod receiver method => callBoundMethod receiver method args
  | .exception _ _ =>
    -- Exception objects are not callable, but exception classes are handled as builtins
    throwTypeError s!"'{typeName callee}' object is not callable"
  | _ => throwTypeError s!"'{typeName callee}' object is not callable"

-- Call a user-defined function
partial def callUserFunc (fd : FuncData) (args : List Value)
    (kwargs : List (String × Value)) : InterpM Value := do
  let scope ← bindFuncParams fd.params args kwargs fd.defaults fd.kwDefaults
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
    -- Type methods like int.from_bytes, bytes.fromhex
    match name, attr with
    | "int", "from_bytes" => return .boundMethod obj attr
    | "bytes", "fromhex" => return .boundMethod obj attr
    | _, _ => throwAttributeError s!"type '{name}' has no attribute '{attr}'"
  | .exception _ _ =>
    match attr with
    | "args" => return .str (Value.toStr obj)
    | _ => throwAttributeError s!"'{typeName obj}' object has no attribute '{attr}'"
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
    throwKeyError (← valueRepr idx)
  | .bytes b =>
    match idx with
    | .int i =>
      let ni := normalizeIndex i b.size
      if ni < 0 || ni >= b.size then throwIndexError "index out of range"
      return .int b[ni.toNat]!.toNat
    | _ => throwTypeError "bytes indices must be integers"
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
  | _ => throwTypeError s!"'{typeName obj}' does not support item assignment"

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
    let result ← evalBinOp op current rhs
    assignToTarget target result

  | .annAssign target _ann value _simple _ => do
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
    let funcVal ← allocFunc (FuncData.mk fd.name fd.args fd.body defaults.toArray kwDefaults.toArray st.localScopes.toArray)
    setVariable fd.name funcVal

  | .asyncFunctionDef fd => do
    -- Treat async as regular for now
    let defaults ← fd.args.defaults.mapM evalExpr
    let kwDefaults ← fd.args.kwDefaults.mapM fun
      | some e => do return some (← evalExpr e)
      | none => return none
    let st ← get
    let funcVal ← allocFunc (FuncData.mk fd.name fd.args fd.body defaults.toArray kwDefaults.toArray st.localScopes.toArray)
    setVariable fd.name funcVal

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
      | _ => throwRuntimeError (.runtimeError "invalid delete target")

  | .global_ names _ => do
    for n in names do declareGlobal n

  | .nonlocal_ names _ => do
    for n in names do declareNonlocal n

  | .classDef cd => do
    -- Minimal: run body in a scope, store as dict
    pushScope {}
    try execStmts cd.body
    catch
    | .control _ => pure ()
    | other => do popScope; throw other
    let st ← get
    let classScope := match st.localScopes with
      | s :: _ => s
      | [] => ({} : Scope)
    popScope
    let mut entries : Array (Value × Value) := #[]
    entries := entries.push (.str "__name__", .str cd.name)
    for (k, v) in classScope do
      entries := entries.push (.str k, v)
    let classVal ← allocDict entries
    setVariable cd.name classVal

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
    -- Evaluate context managers and call __enter__
    let mut managers : List (Value × Value) := []
    for item in items do
      let mgr ← evalExpr item.contextExpr
      -- Try to call __enter__ on the manager
      let entered ← match mgr with
        | .dict ref => do
          let pairs ← heapGetDict ref
          let mut enterFn : Option Value := none
          for (k, v) in pairs do
            match k with
            | .str s => if s == "__enter__" then enterFn := some v
            | _ => pure ()
          match enterFn with
          | some fn => callValueDispatch fn [mgr] []
          | none => pure mgr
        | _ => pure mgr
      if let some target := item.optionalVars then
        assignToTarget target entered
      managers := managers ++ [(mgr, entered)]
    -- Execute body with cleanup
    let mut bodyError : Option Signal := none
    try execStmts body
    catch
    | sig@(.error _) => bodyError := some sig
    | other => do
      -- For control flow signals (return/break), still call __exit__ then re-throw
      for (mgr, _) in managers.reverse do
        match mgr with
        | .dict ref => do
          let pairs ← heapGetDict ref
          for (k, v) in pairs do
            match k with
            | .str s =>
              if s == "__exit__" then do
                let _ ← callValueDispatch v [mgr, .none, .none, .none] []
            | _ => pure ()
        | _ => pure ()
      throw other
    -- Call __exit__ on each manager in reverse order
    let mut suppressed := false
    for (mgr, _) in managers.reverse do
      match mgr with
      | .dict ref => do
        let pairs ← heapGetDict ref
        let mut exitFn : Option Value := none
        for (k, v) in pairs do
          match k with
          | .str s => if s == "__exit__" then exitFn := some v
          | _ => pure ()
        match exitFn with
        | some fn =>
          let exitArgs := match bodyError with
            | none => [mgr, .none, .none, .none]
            | some (.error e) =>
              let excType := Value.str (runtimeErrorTypeName e)
              let excVal := Value.exception (runtimeErrorTypeName e) (runtimeErrorMessage e)
              [mgr, excType, excVal, .none]
            | _ => [mgr, .none, .none, .none]
          let result ← callValueDispatch fn exitArgs []
          if ← isTruthy result then suppressed := true
        | none => pure ()
      | _ => pure ()
    -- Re-raise body error if not suppressed
    match bodyError with
    | some sig => if !suppressed then throw sig
    | none => pure ()
  | .import_ _aliases _ => throwNotImplemented "import statements"
  | .importFrom _ _aliases _ _ => throwNotImplemented "import-from statements"

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
  | _ => throwTypeError s!"'{typeName obj}' does not support item deletion"

-- ============================================================
-- Method call helpers (called by callValueDispatch)
-- ============================================================

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

end

end LeanPython.Interpreter.Eval
