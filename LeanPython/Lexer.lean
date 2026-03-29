import LeanPython.Lexer.Types
import LeanPython.Lexer.Keywords
import LeanPython.Lexer.Operators
import LeanPython.Lexer.State
import LeanPython.Lexer.Char
import LeanPython.Lexer.Indent
import LeanPython.Lexer.Number
import LeanPython.Lexer.StringLit
import LeanPython.Lexer.Core

set_option autoImplicit false

namespace LeanPython.Lexer

/-- Tokenize a Python source string into an array of tokens. -/
def tokenize (source : String) : Except LexError (Array Token) :=
  match lexAll.run (LexerState.initial source) with
  | .ok (tokens, _) => .ok tokens
  | .error err => .error err

end LeanPython.Lexer
