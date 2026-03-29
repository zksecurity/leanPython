import LeanPython.Interpreter.Eval
import LeanPython.Parser.Core

set_option autoImplicit false

namespace LeanPython.Interpreter

open LeanPython.AST (Module)
open LeanPython.Runtime
open LeanPython.Interpreter.Eval
open LeanPython.Parser (parse)

/-- Interpret a Python source string.
    Returns captured output lines on success, or an error message. -/
def interpret (source : String) : IO (Except String (List String)) := do
  match parse source with
  | .error e => return (.error s!"SyntaxError: {e}")
  | .ok (.module stmts) =>
    let initState := InterpreterState.initial
    let result ← (execStmts stmts).run initState
    match result with
    | (.ok (), finalState) => return (.ok finalState.output)
    | (.error (.error e), _) => return (.error (toString e))
    | (.error (.control (.return_ _)), _) =>
      return (.error "SyntaxError: 'return' outside function")
    | (.error (.control .break_), _) =>
      return (.error "SyntaxError: 'break' outside loop")
    | (.error (.control .continue_), _) =>
      return (.error "SyntaxError: 'continue' not properly in loop")

end LeanPython.Interpreter
