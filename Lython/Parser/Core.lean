import Lython.Parser.Stmt
import Lython.Lexer

set_option autoImplicit false

namespace Lython.Parser

open Lython.Lexer (Token TokenKind LexError)
open Lython.AST (Module)

/-- Filter out comment and non-logical newline tokens before parsing. -/
private def filterTokens (tokens : Array Token) : Array Token :=
  tokens.filter fun t =>
    match t.kind with
    | .comment _ | .nlToken => false
    | _ => true

/-- Parse a Python source string into an AST module.
    Returns `Except String Module` for a simple error interface. -/
def parse (source : String) : Except String Module := do
  let tokens ← match Lython.Lexer.tokenize source with
    | .ok ts => .ok ts
    | .error e => .error s!"Lexer error: {repr e}"
  let filtered := filterTokens tokens
  let state : ParserState := { tokens := filtered, pos := 0 }
  match (parseModule).run state with
  | .ok (mod, _) => .ok mod
  | .error e => .error (toString e)

end Lython.Parser
