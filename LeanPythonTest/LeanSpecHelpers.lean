import LeanPython

set_option autoImplicit false

namespace LeanPythonTest.LeanSpecHelpers

open LeanPython.Interpreter

/-- Base path to leanSpec source files. -/
def leanSpecSrc : String := "references/leanSpec/src/lean_spec"

/-- Check if needle is a substring of haystack. -/
def isSubstr (needle haystack : String) : Bool :=
  let nChars := needle.toList
  let hChars := haystack.toList
  let nLen := nChars.length
  let hLen := hChars.length
  if nLen > hLen then false
  else
    (List.range (hLen - nLen + 1)).any fun i =>
      (hChars.drop i |>.take nLen) == nChars

/-- Run Python from a file tree and assert the output matches expected. -/
def assertLeanSpec
    (leanSpecFiles : List (String × String))
    (extraFiles : List (String × String))
    (mainScript : String)
    (expected : String) : IO Unit := do
  let tmpBase := "/tmp/leanpython_leanspec_" ++ toString (← IO.monoNanosNow)
  IO.FS.createDirAll tmpBase
  for (targetRel, srcRel) in leanSpecFiles do
    let content ← IO.FS.readFile (leanSpecSrc ++ "/" ++ srcRel)
    let fullPath := System.FilePath.mk tmpBase / targetRel
    match (System.FilePath.mk tmpBase / targetRel).parent with
    | some parent => IO.FS.createDirAll parent
    | none => pure ()
    IO.FS.writeFile fullPath content
  for (relPath, content) in extraFiles do
    let fullPath := System.FilePath.mk tmpBase / relPath
    match (System.FilePath.mk tmpBase / relPath).parent with
    | some parent => IO.FS.createDirAll parent
    | none => pure ()
    IO.FS.writeFile fullPath content
  let mainPath := (System.FilePath.mk tmpBase / "main.py").toString
  IO.FS.writeFile mainPath mainScript
  let source ← IO.FS.readFile mainPath
  match ← interpret source (some mainPath) with
  | .ok lines =>
    let output := String.join lines
    if output != expected then
      throw (IO.userError s!"Output mismatch!\nExpected: {repr expected}\nGot:      {repr output}")
  | .error msg =>
    throw (IO.userError s!"Runtime error: {msg}")

end LeanPythonTest.LeanSpecHelpers
