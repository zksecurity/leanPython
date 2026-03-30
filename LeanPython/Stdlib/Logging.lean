import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Stdlib.Logging

open LeanPython.Runtime
open LeanPython.Interpreter

-- ============================================================
-- Logging level constants
-- ============================================================

private def levelDEBUG    : Int := 10
private def levelINFO     : Int := 20
private def levelWARNING  : Int := 30

-- ============================================================
-- logging.basicConfig — no-op
-- ============================================================

/-- Python logging.basicConfig(**kwargs): configure logging (no-op). -/
partial def loggingBasicConfig (_args : List Value)
    (_kwargs : List (String × Value)) : InterpM Value := do
  return .none

-- ============================================================
-- Create Logger instance (defined before callLoggerMethod so it can be used)
-- ============================================================

/-- Create a Logger instance. -/
partial def mkLoggerInstance (name : String) (level : Int) : InterpM Value := do
  let mut attrs : Std.HashMap String Value := {}
  attrs := attrs.insert "_name" (.str name)
  attrs := attrs.insert "_level" (.int level)
  attrs := attrs.insert "name" (.str name)
  let cls ← allocClassObj {
    name := "Logger", bases := #[], mro := #[], ns := {}, slots := none }
  match cls with
  | .classObj cref => heapSetClassData cref {
      name := "Logger", bases := #[], mro := #[cls], ns := {}, slots := none }
  | _ => pure ()
  let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
  return .instance instRef

-- ============================================================
-- Logger method dispatch
-- ============================================================

private def levelName (l : Int) : String :=
  if l >= 50 then "CRITICAL"
  else if l >= 40 then "ERROR"
  else if l >= 30 then "WARNING"
  else if l >= 20 then "INFO"
  else "DEBUG"

/-- Dispatch methods on logging.Logger instances. -/
partial def callLoggerMethod (iref : HeapRef) (method : String)
    (args : List Value) : InterpM Value := do
  let id_ ← heapGetInstanceData iref
  let loggerName := match id_.attrs["_name"]? with
    | some (.str n) => n
    | _ => "root"
  let level := match id_.attrs["_level"]? with
    | some (.int n) => n
    | _ => levelWARNING
  match method with
  | "debug" => logAtLevel levelDEBUG level loggerName args
  | "info" => logAtLevel levelINFO level loggerName args
  | "warning" | "warn" => logAtLevel levelWARNING level loggerName args
  | "error" => logAtLevel 40 level loggerName args
  | "critical" => logAtLevel 50 level loggerName args
  | "setLevel" =>
    match args with
    | [.int newLevel] => do
      let newAttrs := id_.attrs.insert "_level" (.int newLevel)
      heapSetInstanceData iref { id_ with attrs := newAttrs }
      return .none
    | _ => throwTypeError "setLevel() requires an integer argument"
  | "getChild" =>
    match args with
    | [.str childName] => do
      let fullName := loggerName ++ "." ++ childName
      mkLoggerInstance fullName level
    | _ => throwTypeError "getChild() requires a string argument"
  | "addHandler" => return .none  -- no-op
  | "isEnabledFor" =>
    match args with
    | [.int msgLevel] => return .bool (msgLevel >= level)
    | _ => return .bool false
  | "getEffectiveLevel" => return .int level
  | "__repr__" => return .str s!"<Logger {loggerName} ({level})>"
  | _ => throwAttributeError s!"'Logger' object has no attribute '{method}'"
where
  logAtLevel (msgLevel curLevel : Int) (lname : String)
      (logArgs : List Value) : InterpM Value := do
    if msgLevel >= curLevel then
      let msg := match logArgs with
        | [.str s] => s
        | [v] => Value.toStr v
        | _ => ""
      emitOutput s!"{levelName msgLevel}:{lname}:{msg}\n"
    return .none

end LeanPython.Stdlib.Logging
