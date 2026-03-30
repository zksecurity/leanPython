import LeanPython.Interpreter.Eval
import LeanPython.Parser.Core

set_option autoImplicit false

namespace LeanPython.Interpreter

open LeanPython.AST (Module)
open LeanPython.Runtime
open LeanPython.Interpreter.Eval
open LeanPython.Parser (parse)

/-- Interpret a Python source string.
    Returns captured output lines on success, or an error message.
    If `filePath` is provided, the directory containing it is used as a search path
    for module imports. -/
def interpret (source : String) (filePath : Option String := none)
    : IO (Except String (List String)) := do
  match parse source with
  | .error e => return (.error s!"SyntaxError: {e}")
  | .ok (.module stmts) =>
    -- Compute search paths from file path
    let searchPaths : Array String := match filePath with
      | some fp =>
        let dir := (System.FilePath.mk fp).parent.getD (System.FilePath.mk ".")
        #[dir.toString]
      | none => #[]
    -- Set up initial global scope with __name__ and __file__
    let mut globalScope : Scope := {}
    globalScope := globalScope.insert "__name__" (.str "__main__")
    match filePath with
    | some fp => globalScope := globalScope.insert "__file__" (.str fp)
    | none => pure ()
    let initState : InterpreterState :=
      { InterpreterState.initial with
        globalScope    := globalScope
        searchPaths    := searchPaths
        currentFile    := filePath
        currentPackage := none }
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
