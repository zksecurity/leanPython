import Lython.Lexer.Types
import Lython.Lexer.Keywords
import Lython.Lexer.Operators
import Lython.Lexer.State
import Lython.Lexer.Char
import Lython.Lexer.Indent
import Lython.Lexer.Number
import Lython.Lexer.StringLit
import Lython.Lexer.Core

set_option autoImplicit false

namespace Lython.Lexer

/-- Tokenize a Python source string into an array of tokens. -/
def tokenize (source : String) : Except LexError (Array Token) :=
  match lexAll.run (LexerState.initial source) with
  | .ok (tokens, _) => .ok tokens
  | .error err => .error err

end Lython.Lexer
