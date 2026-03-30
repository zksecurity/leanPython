import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Stdlib.Pathlib

open LeanPython.Runtime
open LeanPython.Interpreter

-- ============================================================
-- Helpers
-- ============================================================

/-- Check if a file path exists. -/
private def pathExists (path : System.FilePath) : IO Bool := do
  try
    let _ ← IO.FS.readFile path
    return true
  catch _ =>
    -- Try as directory
    try
      let _ ← path.readDir
      return true
    catch _ =>
      return false

/-- Check if a path is a file. -/
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
-- pathlib.Path method dispatch
-- ============================================================

/-- Get the path string from a PurePath instance's _path attribute. -/
private def getPathStr (iref : HeapRef) : InterpM String := do
  let id_ ← heapGetInstanceData iref
  match id_.attrs["_path"]? with
  | some (.str s) => return s
  | _ => return ""

/-- Create a PurePath instance with the given path string. -/
partial def mkPathInstance (path : String) : InterpM Value := do
  let mut attrs : Std.HashMap String Value := {}
  attrs := attrs.insert "_path" (.str path)
  let cls ← allocClassObj {
    name := "PurePath", bases := #[], mro := #[], ns := {}, slots := none }
  match cls with
  | .classObj cref => heapSetClassData cref {
      name := "PurePath", bases := #[], mro := #[cls], ns := {}, slots := none }
  | _ => pure ()
  let instRef ← heapAlloc (.instanceObjData { cls := cls, attrs := attrs })
  return .instance instRef

/-- Dispatch methods and properties on pathlib.Path instances. -/
partial def callPathMethod (iref : HeapRef) (method : String)
    (args : List Value) : InterpM Value := do
  let path ← getPathStr iref
  let fp : System.FilePath := path
  match method with
  | "__str__" | "__repr__" => return .str path
  | "__fspath__" => return .str path
  | "name" =>
    return .str (fp.fileName.getD "")
  | "parent" =>
    let parentStr := match fp.parent with
      | some pp => pp.toString
      | none => "."
    mkPathInstance parentStr
  | "stem" =>
    return .str (fp.fileStem.getD "")
  | "suffix" =>
    match fp.extension with
    | some ext => return .str ("." ++ ext)
    | none => return .str ""
  | "parts" =>
    let parts := path.splitOn "/"
    let filtered := parts.filter (· != "")
    let pfx : List Value := if path.startsWith "/" then [.str "/"] else []
    return .tuple (pfx ++ filtered.map Value.str).toArray
  | "exists" =>
    match args with
    | [] => do
      let e ← (pathExists fp : IO Bool)
      return .bool e
    | _ => throwTypeError "exists() takes no arguments"
  | "is_file" =>
    match args with
    | [] => do
      let e ← (pathIsFile fp : IO Bool)
      return .bool e
    | _ => throwTypeError "is_file() takes no arguments"
  | "is_dir" =>
    match args with
    | [] => do
      let e ← (pathIsDir fp : IO Bool)
      return .bool e
    | _ => throwTypeError "is_dir() takes no arguments"
  | "resolve" =>
    match args with
    | [] => do
      if path.startsWith "/" then
        mkPathInstance path
      else
        let cwd ← (IO.currentDir : IO System.FilePath)
        mkPathInstance (cwd.toString ++ "/" ++ path)
    | _ => throwTypeError "resolve() takes no arguments"
  | "read_text" =>
    match args with
    | [] => do
      let content ← (IO.FS.readFile fp : IO String)
      return .str content
    | _ => throwTypeError "read_text() takes no arguments"
  | "read_bytes" =>
    match args with
    | [] => do
      let content ← (IO.FS.readBinFile fp : IO ByteArray)
      return .bytes content
    | _ => throwTypeError "read_bytes() takes no arguments"
  | "__truediv__" =>
    match args with
    | [.str other] =>
      let joined := if other.startsWith "/" then other
        else if path.endsWith "/" then path ++ other
        else path ++ "/" ++ other
      mkPathInstance joined
    | [.instance otherRef] => do
      let otherPath ← getPathStr otherRef
      let joined := if otherPath.startsWith "/" then otherPath
        else if path.endsWith "/" then path ++ otherPath
        else path ++ "/" ++ otherPath
      mkPathInstance joined
    | _ => throwTypeError "unsupported operand type for /"
  | "with_suffix" =>
    match args with
    | [.str newSuffix] =>
      let stem := fp.fileStem.getD ""
      let parentPart := match fp.parent with
        | some pp => pp.toString ++ "/"
        | none => ""
      mkPathInstance (parentPart ++ stem ++ newSuffix)
    | _ => throwTypeError "with_suffix() requires a string argument"
  | _ => throwAttributeError s!"'PosixPath' object has no attribute '{method}'"

end LeanPython.Stdlib.Pathlib
