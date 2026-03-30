import LeanPython.Runtime.Types
import Std.Data.HashMap
import Std.Data.HashSet

set_option autoImplicit false

namespace LeanPython.Interpreter

open LeanPython.Runtime

-- ============================================================
-- Control flow signals
-- ============================================================

/-- Control flow signals that propagate through the interpreter.
    These are NOT errors -- they are normal Python control flow. -/
inductive ControlFlow where
  | return_   : Value → ControlFlow
  | break_    : ControlFlow
  | continue_ : ControlFlow

/-- A signal is either a runtime error or a control flow event. -/
inductive Signal where
  | error   : RuntimeError → Signal
  | control : ControlFlow → Signal

instance : ToString Signal where
  toString
    | .error e => toString e
    | .control (.return_ v) => s!"Return({v})"
    | .control .break_ => "Break"
    | .control .continue_ => "Continue"

-- ============================================================
-- Interpreter state
-- ============================================================

/-- The mutable state carried by the interpreter. -/
structure InterpreterState where
  globalScope     : Scope
  localScopes     : List Scope
  globalDecls     : List (Std.HashSet String)
  nonlocalDecls   : List (Std.HashSet String)
  heap            : Std.HashMap Nat HeapObject
  nextRef         : Nat
  output          : List String
  activeException : Option RuntimeError
  yieldAccumulator : Option (Array Value)
  loadedModules  : Std.HashMap String Value
  loadingModules : Std.HashSet String
  searchPaths    : Array String
  currentFile    : Option String
  currentPackage : Option String
  deriving Inhabited

/-- Create a fresh interpreter state. -/
def InterpreterState.initial : InterpreterState :=
  { globalScope     := {}
    localScopes     := []
    globalDecls     := []
    nonlocalDecls   := []
    heap            := {}
    nextRef         := 0
    output          := []
    activeException := none
    yieldAccumulator := none
    loadedModules  := {}
    loadingModules := {}
    searchPaths    := #[]
    currentFile    := none
    currentPackage := none }

-- ============================================================
-- Interpreter monad
-- ============================================================

/-- The interpreter monad: ExceptT for errors/control flow,
    StateT for interpreter state, IO for side effects. -/
abbrev InterpM := ExceptT Signal (StateT InterpreterState IO)

-- ============================================================
-- Error/control flow helpers
-- ============================================================

def throwRuntimeError {α : Type} (e : RuntimeError) : InterpM α :=
  throw (.error e)

def throwNameError {α : Type} (msg : String) : InterpM α :=
  throwRuntimeError (.nameError msg)

def throwTypeError {α : Type} (msg : String) : InterpM α :=
  throwRuntimeError (.typeError msg)

def throwValueError {α : Type} (msg : String) : InterpM α :=
  throwRuntimeError (.valueError msg)

def throwIndexError {α : Type} (msg : String) : InterpM α :=
  throwRuntimeError (.indexError msg)

def throwKeyError {α : Type} (msg : String) : InterpM α :=
  throwRuntimeError (.keyError msg)

def throwZeroDivision {α : Type} (msg : String) : InterpM α :=
  throwRuntimeError (.zeroDivision msg)

def throwAssertionError {α : Type} (msg : String) : InterpM α :=
  throwRuntimeError (.assertionError msg)

def throwAttributeError {α : Type} (msg : String) : InterpM α :=
  throwRuntimeError (.attributeError msg)

def throwNotImplemented {α : Type} (msg : String) : InterpM α :=
  throwRuntimeError (.notImplemented msg)

def throwReturn {α : Type} (v : Value) : InterpM α :=
  throw (.control (.return_ v))

def throwBreak {α : Type} : InterpM α :=
  throw (.control .break_)

def throwContinue {α : Type} : InterpM α :=
  throw (.control .continue_)

-- ============================================================
-- Heap operations
-- ============================================================

/-- Allocate a new heap object, returning its reference. -/
def heapAlloc (obj : HeapObject) : InterpM HeapRef := do
  let st ← get
  let ref := st.nextRef
  set { st with heap := st.heap.insert ref obj, nextRef := ref + 1 }
  return ref

/-- Get a heap object by reference. -/
def heapGet (ref : HeapRef) : InterpM HeapObject := do
  let st ← get
  match st.heap[ref]? with
  | some obj => return obj
  | none => throwRuntimeError (.runtimeError s!"invalid heap reference: {ref}")

/-- Update a heap object at the given reference. -/
def heapSet (ref : HeapRef) (obj : HeapObject) : InterpM Unit := do
  modify fun st => { st with heap := st.heap.insert ref obj }

/-- Get a list from the heap. -/
def heapGetList (ref : HeapRef) : InterpM (Array Value) := do
  match ← heapGet ref with
  | .listObj arr => return arr
  | _ => throwRuntimeError (.runtimeError "heap object is not a list")

/-- Set a list on the heap. -/
def heapSetList (ref : HeapRef) (arr : Array Value) : InterpM Unit :=
  heapSet ref (.listObj arr)

/-- Get a dict from the heap. -/
def heapGetDict (ref : HeapRef) : InterpM (Array (Value × Value)) := do
  match ← heapGet ref with
  | .dictObj arr => return arr
  | _ => throwRuntimeError (.runtimeError "heap object is not a dict")

/-- Set a dict on the heap. -/
def heapSetDict (ref : HeapRef) (arr : Array (Value × Value)) : InterpM Unit :=
  heapSet ref (.dictObj arr)

/-- Get a set from the heap. -/
def heapGetSet (ref : HeapRef) : InterpM (Array Value) := do
  match ← heapGet ref with
  | .setObj arr => return arr
  | _ => throwRuntimeError (.runtimeError "heap object is not a set")

/-- Set a set on the heap. -/
def heapSetSet (ref : HeapRef) (arr : Array Value) : InterpM Unit :=
  heapSet ref (.setObj arr)

/-- Get function data from the heap. -/
def heapGetFunc (ref : HeapRef) : InterpM FuncData := do
  match ← heapGet ref with
  | .funcObj fd => return fd
  | _ => throwRuntimeError (.runtimeError "heap object is not a function")

-- ============================================================
-- Allocate mutable containers
-- ============================================================

/-- Allocate a new list on the heap. -/
def allocList (arr : Array Value) : InterpM Value := do
  let ref ← heapAlloc (.listObj arr)
  return .list ref

/-- Allocate a new dict on the heap. -/
def allocDict (arr : Array (Value × Value)) : InterpM Value := do
  let ref ← heapAlloc (.dictObj arr)
  return .dict ref

/-- Allocate a new set on the heap. -/
def allocSet (arr : Array Value) : InterpM Value := do
  let ref ← heapAlloc (.setObj arr)
  return .set ref

/-- Allocate a function on the heap. -/
def allocFunc (fd : FuncData) : InterpM Value := do
  let ref ← heapAlloc (.funcObj fd)
  return .function ref

-- ============================================================
-- Scope operations
-- ============================================================

/-- Check if we are inside a function (have local scopes). -/
def inFunction : InterpM Bool := do
  return !(← get).localScopes.isEmpty

/-- Push a new local scope. -/
def pushScope (scope : Scope) (globals : Std.HashSet String := {})
    (nonlocals : Std.HashSet String := {}) : InterpM Unit :=
  modify fun st => { st with
    localScopes   := scope :: st.localScopes
    globalDecls   := globals :: st.globalDecls
    nonlocalDecls := nonlocals :: st.nonlocalDecls }

/-- Pop the innermost local scope. -/
def popScope : InterpM Unit :=
  modify fun st => { st with
    localScopes   := st.localScopes.tail
    globalDecls   := st.globalDecls.tail
    nonlocalDecls := st.nonlocalDecls.tail }

-- ============================================================
-- Variable lookup and assignment
-- ============================================================

/-- Look up a variable by name following Python's LEGB rule (simplified). -/
def lookupVariable (name : String) : InterpM Value := do
  let st ← get
  -- Local scopes (innermost first)
  for scope in st.localScopes do
    if let some v := scope[name]? then return v
  -- Global scope
  if let some v := st.globalScope[name]? then return v
  -- Builtins
  if isBuiltinName name then return .builtin name
  throwNameError s!"name '{name}' is not defined"

/-- Check if the current scope has declared `name` as global. -/
def isDeclaredGlobal (name : String) : InterpM Bool := do
  let st ← get
  match st.globalDecls with
  | [] => return false
  | gs :: _ => return gs.contains name

/-- Check if the current scope has declared `name` as nonlocal. -/
def isDeclaredNonlocal (name : String) : InterpM Bool := do
  let st ← get
  match st.nonlocalDecls with
  | [] => return false
  | ns :: _ => return ns.contains name

/-- Set a variable in the appropriate scope. -/
def setVariable (name : String) (value : Value) : InterpM Unit := do
  let st ← get
  -- If declared global, set in global scope
  if ← isDeclaredGlobal name then
    set { st with globalScope := st.globalScope.insert name value }
    return
  -- If declared nonlocal, search enclosing scopes
  if ← isDeclaredNonlocal name then
    let st' ← get
    match st'.localScopes with
    | [] => set { st' with globalScope := st'.globalScope.insert name value }
    | _ :: enclosing =>
      let rec setInEnclosing (scopes : List Scope) : List Scope :=
        match scopes with
        | [] => []
        | s :: rest =>
          if s.contains name then (s.insert name value) :: rest
          else s :: setInEnclosing rest
      set { st' with localScopes := st'.localScopes.head! :: setInEnclosing enclosing }
    return
  -- Otherwise set in innermost local scope, or global if no locals
  match st.localScopes with
  | [] => set { st with globalScope := st.globalScope.insert name value }
  | s :: rest => set { st with localScopes := (s.insert name value) :: rest }

/-- Delete a variable from the current scope. -/
def deleteVariable (name : String) : InterpM Unit := do
  let st ← get
  match st.localScopes with
  | [] => set { st with globalScope := st.globalScope.erase name }
  | s :: rest => set { st with localScopes := (s.erase name) :: rest }

/-- Register a global declaration in the current scope. -/
def declareGlobal (name : String) : InterpM Unit :=
  modify fun st =>
    match st.globalDecls with
    | [] => st
    | gs :: rest => { st with globalDecls := (gs.insert name) :: rest }

/-- Register a nonlocal declaration in the current scope. -/
def declareNonlocal (name : String) : InterpM Unit :=
  modify fun st =>
    match st.nonlocalDecls with
    | [] => st
    | ns :: rest => { st with nonlocalDecls := (ns.insert name) :: rest }

-- ============================================================
-- Output capture
-- ============================================================

/-- Append a string to captured output (for testing) and print to stdout. -/
def emitOutput (s : String) : InterpM Unit := do
  modify fun st => { st with output := st.output ++ [s] }
  (IO.print s : IO Unit)

-- ============================================================
-- Type name helper
-- ============================================================

/-- Get the Python type name of a value. -/
def typeName : Value → String
  | .none => "NoneType"
  | .bool _ => "bool"
  | .int _ => "int"
  | .float _ => "float"
  | .str _ => "str"
  | .bytes _ => "bytes"
  | .list _ => "list"
  | .tuple _ => "tuple"
  | .dict _ => "dict"
  | .set _ => "set"
  | .function _ => "function"
  | .builtin _ => "builtin_function_or_method"
  | .ellipsis => "ellipsis"
  | .boundMethod _ _ => "method"
  | .exception tn _ => tn
  | .generator _ => "generator"
  | .classObj _ => "type"
  | .instance _ => "instance"
  | .superObj _ _ => "super"
  | .staticMethod _ => "staticmethod"
  | .classMethod _ => "classmethod"
  | .property _ _ _ => "property"
  | .module _ => "module"

-- ============================================================
-- Class heap operations
-- ============================================================

/-- Get class data from the heap. -/
def heapGetClassData (ref : HeapRef) : InterpM ClassData := do
  match ← heapGet ref with
  | .classObjData cd => return cd
  | _ => throwRuntimeError (.runtimeError "heap object is not a class")

/-- Update class data on the heap. -/
def heapSetClassData (ref : HeapRef) (cd : ClassData) : InterpM Unit :=
  heapSet ref (.classObjData cd)

/-- Get instance data from the heap. -/
def heapGetInstanceData (ref : HeapRef) : InterpM InstanceData := do
  match ← heapGet ref with
  | .instanceObjData id_ => return id_
  | _ => throwRuntimeError (.runtimeError "heap object is not an instance")

/-- Update instance data on the heap. -/
def heapSetInstanceData (ref : HeapRef) (id_ : InstanceData) : InterpM Unit :=
  heapSet ref (.instanceObjData id_)

/-- Allocate a class object on the heap. -/
def allocClassObj (cd : ClassData) : InterpM Value := do
  let ref ← heapAlloc (.classObjData cd)
  return .classObj ref

/-- Allocate an instance on the heap. -/
def allocInstance (id_ : InstanceData) : InterpM Value := do
  let ref ← heapAlloc (.instanceObjData id_)
  return .instance ref

-- ============================================================
-- Generator heap operations
-- ============================================================

/-- Get a generator object from the heap. -/
def heapGetGenerator (ref : HeapRef) : InterpM (Array Value × Nat) := do
  match ← heapGet ref with
  | .generatorObj buf idx => return (buf, idx)
  | _ => throwRuntimeError (.runtimeError "heap object is not a generator")

/-- Update generator index on the heap. -/
def heapSetGeneratorIdx (ref : HeapRef) (newIdx : Nat) : InterpM Unit := do
  match ← heapGet ref with
  | .generatorObj buf _ => heapSet ref (.generatorObj buf newIdx)
  | _ => throwRuntimeError (.runtimeError "heap object is not a generator")

/-- Allocate a generator on the heap. -/
def allocGenerator (values : Array Value) : InterpM Value := do
  let ref ← heapAlloc (.generatorObj values 0)
  return .generator ref

-- ============================================================
-- Module heap operations
-- ============================================================

/-- Get module data from the heap. -/
def heapGetModuleData (ref : HeapRef) : InterpM ModuleData := do
  match ← heapGet ref with
  | .moduleObj md => return md
  | _ => throwRuntimeError (.runtimeError "heap object is not a module")

/-- Update module data on the heap. -/
def heapSetModuleData (ref : HeapRef) (md : ModuleData) : InterpM Unit :=
  heapSet ref (.moduleObj md)

/-- Allocate a module on the heap. -/
def allocModule (md : ModuleData) : InterpM Value := do
  let ref ← heapAlloc (.moduleObj md)
  return .module ref

-- ============================================================
-- Import error helpers
-- ============================================================

def throwImportError {α : Type} (msg : String) : InterpM α :=
  throwRuntimeError (.importError msg)

def throwModuleNotFoundError {α : Type} (name : String) : InterpM α :=
  throwRuntimeError (.moduleNotFound name)

end LeanPython.Interpreter
