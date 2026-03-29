import Lython.Lexer.Types

set_option autoImplicit false

namespace Lython.Parser

open Lython.Lexer (SourcePos SourceSpan Token TokenKind)

/-- Parser error types. -/
inductive ParseError where
  | unexpectedToken : TokenKind → SourcePos → String → ParseError
  | unexpectedEOF   : String → ParseError
  | expectedToken   : String → TokenKind → SourcePos → ParseError
  | invalidSyntax   : String → SourceSpan → ParseError
  deriving Repr, Inhabited

instance : ToString ParseError where
  toString
    | .unexpectedToken tk pos msg =>
      s!"Unexpected token {repr tk} at {pos.line}:{pos.column}: {msg}"
    | .unexpectedEOF msg =>
      s!"Unexpected end of input: {msg}"
    | .expectedToken expected got pos =>
      s!"Expected {expected}, got {repr got} at {pos.line}:{pos.column}"
    | .invalidSyntax msg span =>
      s!"Invalid syntax at {span.start.line}:{span.start.column}: {msg}"

/-- Parser state: a filtered token array and the current position index. -/
structure ParserState where
  tokens : Array Token
  pos    : Nat
  deriving Inhabited

/-- The parser monad: StateT over Except for backtracking. -/
abbrev ParserM := StateT ParserState (Except ParseError)

end Lython.Parser
