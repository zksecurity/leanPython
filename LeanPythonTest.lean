import LeanPythonTest.Basic
import LeanPythonTest.Parser
import LeanPythonTest.Interpreter
import LeanPythonTest.Module
import LeanPythonTest.Stdlib
import LeanPythonTest.Crypto
-- LeanSpec e2e tests are too slow for #eval (they use the interpreter at
-- elaboration time). They are tested via `lake exe leanPython` in CI instead.
-- import LeanPythonTest.LeanSpec
