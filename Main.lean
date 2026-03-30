import LeanPython

def main (args : List String) : IO UInt32 := do
  match args with
  | [path] =>
    let source ← IO.FS.readFile path
    match ← LeanPython.Interpreter.interpret source (some path) with
    | .ok _ => return 0
    | .error msg =>
      IO.eprintln msg
      return 1
  | _ =>
    IO.println "Usage: leanPython <file.py>"
    return 1
