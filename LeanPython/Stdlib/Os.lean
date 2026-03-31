import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Stdlib.Os

open LeanPython.Runtime
open LeanPython.Interpreter

-- ============================================================
-- Helpers
-- ============================================================

/-- Check if a file path exists (file or directory). -/
private def pathExists (path : System.FilePath) : IO Bool := do
  try
    let _ ← IO.FS.readFile path
    return true
  catch _ =>
    try
      let _ ← path.readDir
      return true
    catch _ =>
      return false

/-- Check if a path is a regular file. -/
private def pathIsFile (path : System.FilePath) : IO Bool := do
  try
    let _ ← IO.FS.readFile path
    return true
  catch _ =>
    return false

/-- Check if a path is a directory. -/
private def pathIsDir (path : System.FilePath) : IO Bool := do
  try
    let _ ← path.readDir
    return true
  catch _ =>
    return false

-- ============================================================
-- os module functions
-- ============================================================

/-- Python os.urandom(n): return n random bytes. -/
partial def osUrandom (args : List Value) : InterpM Value := do
  match args with
  | [.int n] =>
    if n < 0 then throwValueError "negative argument not allowed"
    else do
      let mut ba := ByteArray.empty
      for _ in [:n.toNat] do
        let b ← (IO.rand 0 255 : IO Nat)
        ba := ba.push b.toUInt8
      return .bytes ba
  | _ => throwTypeError "urandom() takes exactly 1 argument"

/-- Python os.getcwd(): return current working directory. -/
partial def osGetcwd (args : List Value) : InterpM Value := do
  match args with
  | [] =>
    let cwd ← (IO.currentDir : IO System.FilePath)
    return .str cwd.toString
  | _ => throwTypeError "getcwd() takes no arguments"

/-- Python os.getenv(name, default=None). -/
partial def osGetenv (args : List Value) : InterpM Value := do
  match args with
  | [.str name] => do
    let val ← (IO.getEnv name : IO (Option String))
    match val with
    | some v => return .str v
    | none => return .none
  | [.str name, dflt] => do
    let val ← (IO.getEnv name : IO (Option String))
    match val with
    | some v => return .str v
    | none => return dflt
  | _ => throwTypeError "getenv() requires a string argument"

/-- Python os.listdir(path='.'). -/
partial def osListdir (args : List Value) : InterpM Value := do
  let pathStr := match args with
    | [.str p] => p
    | [] => "."
    | _ => "."
  let entries ← (do
    let dir : System.FilePath := pathStr
    let mut result : Array Value := #[]
    let contents ← dir.readDir
    for entry in contents do
      result := result.push (.str entry.fileName)
    pure result : IO (Array Value))
  allocList entries

-- ============================================================
-- os.path module functions
-- ============================================================

/-- Python os.path.join(a, b, ...): join path components. -/
partial def osPathJoin (args : List Value) : InterpM Value := do
  match args with
  | [] => throwTypeError "join() requires at least one argument"
  | _ => do
    let mut result := ""
    for arg in args do
      match arg with
      | .str s =>
        if result.isEmpty then
          result := s
        else if s.startsWith "/" then
          result := s  -- absolute path resets
        else if result.endsWith "/" then
          result := result ++ s
        else
          result := result ++ "/" ++ s
      | _ => throwTypeError "join() argument must be str"
    return .str result

/-- Python os.path.exists(path): check if path exists. -/
partial def osPathExists (args : List Value) : InterpM Value := do
  match args with
  | [.str path] => do
    let e ← (pathExists path : IO Bool)
    return .bool e
  | _ => throwTypeError "exists() requires a string argument"

/-- Python os.path.isfile(path). -/
partial def osPathIsfile (args : List Value) : InterpM Value := do
  match args with
  | [.str path] => do
    let e ← (pathIsFile path : IO Bool)
    return .bool e
  | _ => throwTypeError "isfile() requires a string argument"

/-- Python os.path.isdir(path). -/
partial def osPathIsdir (args : List Value) : InterpM Value := do
  match args with
  | [.str path] => do
    let e ← (pathIsDir path : IO Bool)
    return .bool e
  | _ => throwTypeError "isdir() requires a string argument"

/-- Helper to split a path string into directory and base. -/
private def splitPath (path : String) : String × String :=
  let p : System.FilePath := path
  let parentPart := match p.parent with
    | some pp => pp.toString
    | none => ""
  let basePart := p.fileName.getD ""
  (parentPart, basePart)

/-- Python os.path.dirname(path). -/
partial def osPathDirname (args : List Value) : InterpM Value := do
  match args with
  | [.str path] => return .str (splitPath path).1
  | _ => throwTypeError "dirname() requires a string argument"

/-- Python os.path.basename(path). -/
partial def osPathBasename (args : List Value) : InterpM Value := do
  match args with
  | [.str path] => return .str (splitPath path).2
  | _ => throwTypeError "basename() requires a string argument"

/-- Python os.path.abspath(path). -/
partial def osPathAbspath (args : List Value) : InterpM Value := do
  match args with
  | [.str path] => do
    if path.startsWith "/" then
      return .str path
    else
      let cwd ← (IO.currentDir : IO System.FilePath)
      let full := cwd.toString ++ "/" ++ path
      return .str full
  | _ => throwTypeError "abspath() requires a string argument"

/-- Python os.path.splitext(path): split into (root, ext). -/
partial def osPathSplitExt (args : List Value) : InterpM Value := do
  match args with
  | [.str path] =>
    let p : System.FilePath := path
    match p.extension with
    | some ext =>
      let root := String.ofList (path.toList.take (path.length - ext.length - 1))
      return .tuple #[.str root, .str ("." ++ ext)]
    | none => return .tuple #[.str path, .str ""]
  | _ => throwTypeError "splitext() requires a string argument"

/-- Python os.path.normpath(path). -/
partial def osPathNormpath (args : List Value) : InterpM Value := do
  match args with
  | [.str path] => return .str path  -- simplified: just return as-is
  | _ => throwTypeError "normpath() requires a string argument"

end LeanPython.Stdlib.Os
