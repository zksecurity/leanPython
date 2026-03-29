import LeanPython.Parser.Stmt
import LeanPython.Lexer

set_option autoImplicit false

namespace LeanPython.Parser

open LeanPython.Lexer (Token TokenKind LexError)
open LeanPython.AST (Module)

/-- Filter out comment and non-logical newline tokens before parsing. -/
private def filterTokens (tokens : Array Token) : Array Token :=
  tokens.filter fun t =>
    match t.kind with
    | .comment _ | .nlToken => false
    | _ => true

/-- Parse a Python source string into an AST module.
    Returns `Except String Module` for a simple error interface. -/
def parse (source : String) : Except String Module := do
  let tokens ← match LeanPython.Lexer.tokenize source with
    | .ok ts => .ok ts
    | .error e => .error s!"Lexer error: {repr e}"
  let filtered := filterTokens tokens
  let state : ParserState := { tokens := filtered, pos := 0 }
  match (parseModule).run state with
  | .ok (mod, _) => .ok mod
  | .error e => .error (toString e)

end LeanPython.Parser
