import LeanPython

set_option autoImplicit false

open LeanPython.Interpreter

-- ============================================================
-- Test helpers
-- ============================================================

/-- Run Python source and assert the output matches expected. -/
private def assertPy (source expected : String) : IO Unit := do
  match ← interpret source with
  | .ok lines =>
    let output := String.join lines
    if output != expected then
      throw (IO.userError s!"Output mismatch!\nExpected: {repr expected}\nGot:      {repr output}")
  | .error msg =>
    throw (IO.userError s!"Runtime error: {msg}")

/-- Check if needle is a substring of haystack. -/
private def isSubstr (needle haystack : String) : Bool :=
  let nChars := needle.toList
  let hChars := haystack.toList
  let nLen := nChars.length
  let hLen := hChars.length
  if nLen > hLen then false
  else
    (List.range (hLen - nLen + 1)).any fun i =>
      (hChars.drop i |>.take nLen) == nChars

/-- Run Python source and assert it produces a runtime error containing msg. -/
private def assertPyError (source errSubstr : String) : IO Unit := do
  match ← interpret source with
  | .ok lines =>
    throw (IO.userError s!"Expected error containing '{errSubstr}', got output: {String.join lines}")
  | .error msg =>
    if !isSubstr errSubstr msg then
      throw (IO.userError s!"Expected error containing '{errSubstr}', got: {msg}")

/-- Run Python source from a file and assert the output matches expected.
    Creates the file tree, runs the main file, then cleans up. -/
private def assertPyFiles (files : List (String × String)) (mainFile : String)
    (expected : String) : IO Unit := do
  -- Create temp directory
  let tmpBase := "/tmp/leanpython_test_" ++ toString (← IO.monoNanosNow)
  IO.FS.createDirAll tmpBase
  -- Write all files
  for (relPath, content) in files do
    let fullPath := System.FilePath.mk tmpBase / relPath
    -- Ensure parent directory exists
    match (System.FilePath.mk tmpBase / relPath).parent with
    | some parent => IO.FS.createDirAll parent
    | none => pure ()
    IO.FS.writeFile fullPath content
  -- Run the main file
  let mainPath := (System.FilePath.mk tmpBase / mainFile).toString
  let source ← IO.FS.readFile mainPath
  match ← interpret source (some mainPath) with
  | .ok lines =>
    let output := String.join lines
    if output != expected then
      throw (IO.userError s!"Output mismatch!\nExpected: {repr expected}\nGot:      {repr output}")
  | .error msg =>
    throw (IO.userError s!"Runtime error: {msg}")

/-- Run Python source from a file and assert it produces a runtime error. -/
private def assertPyFilesError (files : List (String × String)) (mainFile : String)
    (errSubstr : String) : IO Unit := do
  let tmpBase := "/tmp/leanpython_test_" ++ toString (← IO.monoNanosNow)
  IO.FS.createDirAll tmpBase
  for (relPath, content) in files do
    let fullPath := System.FilePath.mk tmpBase / relPath
    match (System.FilePath.mk tmpBase / relPath).parent with
    | some parent => IO.FS.createDirAll parent
    | none => pure ()
    IO.FS.writeFile fullPath content
  let mainPath := (System.FilePath.mk tmpBase / mainFile).toString
  let source ← IO.FS.readFile mainPath
  match ← interpret source (some mainPath) with
  | .ok lines =>
    throw (IO.userError s!"Expected error containing '{errSubstr}', got output: {String.join lines}")
  | .error msg =>
    if !isSubstr errSubstr msg then
      throw (IO.userError s!"Expected error containing '{errSubstr}', got: {msg}")

-- ============================================================
-- from __future__ import annotations (no-op)
-- ============================================================

#eval assertPy "from __future__ import annotations\nprint('ok')\n" "ok\n"

-- ============================================================
-- typing module
-- ============================================================

#eval assertPy "from typing import TYPE_CHECKING\nprint(TYPE_CHECKING)\n" "False\n"

#eval assertPy "from typing import TYPE_CHECKING\nif TYPE_CHECKING:\n    print('skip')\nprint('done')\n" "done\n"

#eval assertPy "from typing import Any, Optional, List\nprint('ok')\n" "ok\n"

#eval assertPy "import typing\nprint(typing.TYPE_CHECKING)\n" "False\n"

-- ============================================================
-- __name__ attribute for main script
-- ============================================================

#eval assertPy "print(__name__)\n" "__main__\n"

-- ============================================================
-- Basic module import (file-based)
-- ============================================================

-- import helper
#eval assertPyFiles
  [("helper.py", "x = 42\ndef greet():\n    return 'hello'\n"),
   ("main.py", "import helper\nprint(helper.x)\nprint(helper.greet())\n")]
  "main.py"
  "42\nhello\n"

-- from helper import x
#eval assertPyFiles
  [("helper.py", "x = 42\ny = 99\n"),
   ("main.py", "from helper import x\nprint(x)\n")]
  "main.py"
  "42\n"

-- from helper import x as z
#eval assertPyFiles
  [("helper.py", "x = 42\n"),
   ("main.py", "from helper import x as z\nprint(z)\n")]
  "main.py"
  "42\n"

-- import helper as h
#eval assertPyFiles
  [("helper.py", "x = 42\n"),
   ("main.py", "import helper as h\nprint(h.x)\n")]
  "main.py"
  "42\n"

-- ============================================================
-- from module import *
-- ============================================================

-- Without __all__ (imports all non-underscore names)
#eval assertPyFiles
  [("helper.py", "x = 1\ny = 2\n_private = 3\n"),
   ("main.py", "from helper import *\nprint(x)\nprint(y)\n")]
  "main.py"
  "1\n2\n"

-- With __all__
#eval assertPyFiles
  [("helper.py", "__all__ = ['x']\nx = 1\ny = 2\n"),
   ("main.py", "from helper import *\nprint(x)\n")]
  "main.py"
  "1\n"

-- ============================================================
-- Package imports (__init__.py)
-- ============================================================

#eval assertPyFiles
  [("pkg/__init__.py", "name = 'pkg'\n"),
   ("pkg/sub.py", "val = 100\n"),
   ("main.py", "import pkg\nprint(pkg.name)\n")]
  "main.py"
  "pkg\n"

#eval assertPyFiles
  [("pkg/__init__.py", ""),
   ("pkg/sub.py", "val = 100\n"),
   ("main.py", "from pkg import sub\nprint(sub.val)\n")]
  "main.py"
  "100\n"

#eval assertPyFiles
  [("pkg/__init__.py", ""),
   ("pkg/sub.py", "val = 100\n"),
   ("main.py", "from pkg.sub import val\nprint(val)\n")]
  "main.py"
  "100\n"

-- ============================================================
-- Module caching (import same module twice)
-- ============================================================

#eval assertPyFiles
  [("counter.py", "count = 0\ncount = count + 1\nprint('loaded')\n"),
   ("main.py", "import counter\nimport counter\nprint(counter.count)\n")]
  "main.py"
  "loaded\n1\n"

-- ============================================================
-- __name__ for imported modules
-- ============================================================

#eval assertPyFiles
  [("helper.py", "print(__name__)\n"),
   ("main.py", "import helper\n")]
  "main.py"
  "helper\n"

-- ============================================================
-- Module not found error
-- ============================================================

#eval assertPyFilesError
  [("main.py", "import nonexistent\n")]
  "main.py"
  "ModuleNotFoundError"

-- ============================================================
-- ImportError for missing name
-- ============================================================

#eval assertPyFilesError
  [("helper.py", "x = 1\n"),
   ("main.py", "from helper import missing_name\n")]
  "main.py"
  "ImportError"

-- ============================================================
-- Relative imports
-- ============================================================

#eval assertPyFiles
  [("pkg/__init__.py", ""),
   ("pkg/a.py", "from .b import val\nresult = val + 1\n"),
   ("pkg/b.py", "val = 10\n"),
   ("main.py", "from pkg import a\nprint(a.result)\n")]
  "main.py"
  "11\n"

-- ============================================================
-- Module with classes and functions
-- ============================================================

#eval assertPyFiles
  [("shapes.py", "class Circle:\n    def __init__(self, r):\n        self.r = r\n    def area(self):\n        return 3 * self.r * self.r\n"),
   ("main.py", "from shapes import Circle\nc = Circle(5)\nprint(c.area())\n")]
  "main.py"
  "75\n"

-- ============================================================
-- typing_extensions and abc synthetic modules
-- ============================================================

#eval assertPy "from typing_extensions import override\nprint('ok')\n" "ok\n"

#eval assertPy "from abc import ABC, abstractmethod\nprint('ok')\n" "ok\n"

#eval assertPy "from dataclasses import dataclass\nprint('ok')\n" "ok\n"

#eval assertPy "from functools import lru_cache\nprint('ok')\n" "ok\n"

#eval assertPy "from enum import Enum\nprint('ok')\n" "ok\n"
