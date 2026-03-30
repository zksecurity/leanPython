import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Stdlib.Sys

open LeanPython.Runtime
open LeanPython.Interpreter

-- ============================================================
-- sys.exit — raises SystemExit
-- ============================================================

/-- Python sys.exit(code): raise SystemExit. -/
partial def sysExit (args : List Value) : InterpM Value := do
  let code := match args with
    | [] => 0
    | [.int n] => n
    | [.none] => 0
    | _ => 1
  throwRuntimeError (.runtimeError s!"SystemExit: {code}")

-- ============================================================
-- TextIOWrapper method dispatch (for sys.stdout / sys.stderr)
-- ============================================================

/-- Dispatch methods on sys.stdout / sys.stderr TextIOWrapper instances. -/
partial def callTextIOWrapperMethod (iref : HeapRef) (method : String)
    (args : List Value) : InterpM Value := do
  let id_ ← heapGetInstanceData iref
  let streamName := match id_.attrs["_name"]? with
    | some (.str n) => n
    | _ => "stdout"
  match method with
  | "write" =>
    match args with
    | [.str s] => do
      if streamName == "stderr" then
        -- stderr: still emit to output for now
        emitOutput s
      else
        emitOutput s
      return .int s.length
    | _ => throwTypeError "write() argument must be str"
  | "flush" => return .none
  | "fileno" =>
    if streamName == "stdout" then return .int 1
    else if streamName == "stderr" then return .int 2
    else return .int 0
  | "isatty" => return .bool false
  | "readable" => return .bool false
  | "writable" => return .bool true
  | "seekable" => return .bool false
  | "close" => return .none
  | "__enter__" => return .instance iref
  | "__exit__" => return .none
  | _ => throwAttributeError s!"'TextIOWrapper' object has no attribute '{method}'"

end LeanPython.Stdlib.Sys
