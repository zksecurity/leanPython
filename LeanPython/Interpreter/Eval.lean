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
    if name.startsWith "__list_method_" then return ← callListMethodByName name args
    else if name.startsWith "__dict_method_" then return ← callDictMethodByName name args
    else if name.startsWith "__str_method_" then return ← callStrMethodByName name args
    else if name.startsWith "__set_method_" then return ← callSetMethodByName name args
    else callBuiltin name args kwargs
  | .function ref => do
    let fd ← heapGetFunc ref
    callUserFunc fd args kwargs
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
  | .str _ => return .builtin s!"__str_method_{attr}_{obj.toStr}"
  | .list ref => return .builtin s!"__list_method_{attr}_{ref}"
  | .dict ref => return .builtin s!"__dict_method_{attr}_{ref}"
  | .set ref => return .builtin s!"__set_method_{attr}_{ref}"
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
  | .attribute _obj _attr _ => throwNotImplemented "attribute assignment"
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

  | .raise_ exprOpt _cause _ => do
    match exprOpt with
    | some e => throwRuntimeError (.runtimeError (← valueToStr (← evalExpr e)))
    | none => throwRuntimeError (.runtimeError "re-raise without active exception")

  | .try_ body handlers orelse finally_ _ => do
    let bodyResult ← do
      try
        execStmts body
        pure (none : Option Signal)
      catch
      | sig@(.error _) => pure (some sig)
      | other => throw other
    match bodyResult with
    | none => execStmts orelse
    | some (.error e) => do
      let mut handled := false
      for handler in handlers do
        match handler with
        | .mk _type_ handlerName handlerBody _ => do
          if let some n := handlerName then
            setVariable n (.str (toString e))
          execStmts handlerBody
          handled := true
          break
      if !handled then throwRuntimeError e
    | some other => throw other
    execStmts finally_

  | .with_ _items _body _ => throwNotImplemented "with statements"
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

partial def callListMethodByName (encodedName : String) (args : List Value)
    : InterpM Value := do
  -- Encoded: __list_method_METHOD_REF
  let rest := strDrop encodedName "__list_method_".length
  -- Find last underscore to split method from ref
  let parts := rest.toList
  let revParts := parts.reverse
  let refChars := revParts.takeWhile Char.isDigit |>.reverse
  let methodChars := parts.take (parts.length - refChars.length - 1) -- -1 for separator _
  let method := String.ofList methodChars
  let refStr := String.ofList refChars
  match refStr.toNat? with
  | some ref => callListMethod ref method args
  | none => throwNotImplemented s!"method {encodedName}"

partial def callDictMethodByName (encodedName : String) (args : List Value)
    : InterpM Value := do
  let rest := strDrop encodedName "__dict_method_".length
  let parts := rest.toList
  let revParts := parts.reverse
  let refChars := revParts.takeWhile Char.isDigit |>.reverse
  let methodChars := parts.take (parts.length - refChars.length - 1)
  let method := String.ofList methodChars
  let refStr := String.ofList refChars
  match refStr.toNat? with
  | some ref => callDictMethod ref method args
  | none => throwNotImplemented s!"method {encodedName}"

partial def callStrMethodByName (encodedName : String) (args : List Value)
    : InterpM Value := do
  -- Encoded: __str_method_METHOD_ACTUALSTRING
  -- Find the method name between first and second underscore groups
  let rest := strDrop encodedName "__str_method_".length
  -- Split on first _ to get method, rest is the string
  match rest.toList.dropWhile (· != '_') with
  | [] => throwNotImplemented s!"method {encodedName}"
  | _ :: strChars =>
    let methodChars := rest.toList.takeWhile (· != '_')
    let method := String.ofList methodChars
    let s := String.ofList strChars
    callStrMethod s method args

partial def callSetMethodByName (encodedName : String) (args : List Value)
    : InterpM Value := do
  let rest := strDrop encodedName "__set_method_".length
  let parts := rest.toList
  let revParts := parts.reverse
  let refChars := revParts.takeWhile Char.isDigit |>.reverse
  let methodChars := parts.take (parts.length - refChars.length - 1)
  let method := String.ofList methodChars
  let refStr := String.ofList refChars
  match refStr.toNat? with
  | some ref => callSetMethod ref method args
  | none => throwNotImplemented s!"method {encodedName}"

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
  | _ => throwNotImplemented s!"list.{method}()"

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
  | _ => throwNotImplemented s!"dict.{method}()"

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
  | "format" => return .str s
  | "count" =>
    match args with
    | [.str sub] => return .int (stringCount s sub)
    | _ => throwTypeError "count() takes 1 argument"
  | _ => throwNotImplemented s!"str.{method}()"
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
  | _ => throwNotImplemented s!"set.{method}()"

end

end LeanPython.Interpreter.Eval
