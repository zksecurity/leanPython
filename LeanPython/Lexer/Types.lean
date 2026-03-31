import LeanPython.Lexer.Keywords
import LeanPython.Lexer.Operators

set_option autoImplicit false

namespace LeanPython.Lexer

/-- Source position in a Python file. -/
structure SourcePos where
  line   : Nat  -- 1-based line number
  column : Nat  -- 0-based column
  offset : Nat  -- 0-based byte offset from start
  deriving Repr, BEq, Inhabited

/-- Source span (start to stop positions). -/
structure SourceSpan where
  start : SourcePos
  stop  : SourcePos
  deriving Repr, BEq, Inhabited

/-- Lexer error types. -/
inductive LexError where
  | unexpectedChar     : Char → SourcePos → LexError
  | unterminatedString : SourcePos → LexError
  | invalidEscape      : Char → SourcePos → LexError
  | invalidNumber      : String → SourcePos → LexError
  | inconsistentIndent : SourcePos → LexError
  | mixedTabsSpaces    : SourcePos → LexError
  | unexpectedEOF      : SourcePos → LexError
  deriving Repr, BEq, Inhabited

instance : Repr ByteArray where
  reprPrec ba _ := repr ba.toList

/-- A part of an f-string: either literal text or an expression with optional conversion. -/
inductive FStringPart where
  | literal : String → FStringPart
  | expr    : String → Option Char → FStringPart  -- expression source, conversion flag (!r !s !a)
  deriving Repr, BEq

/-- Python token kind. -/
inductive TokenKind where
  | name          : String → TokenKind
  | integer       : Int → TokenKind
  | float_        : Float → TokenKind
  | imaginary     : Float → TokenKind
  | string        : String → TokenKind
  | bytes         : ByteArray → TokenKind
  | fstringToken  : Array FStringPart → TokenKind
  | keyword       : Keyword → TokenKind
  | operator      : Operator → TokenKind
  | delimiter     : Delimiter → TokenKind
  | indent        : TokenKind
  | dedent        : TokenKind
  | newline       : TokenKind
  | nlToken       : TokenKind
  | endmarker     : TokenKind
  | comment       : String → TokenKind
  deriving Repr

instance : BEq TokenKind where
  beq
    | .name a, .name b => a == b
    | .integer a, .integer b => a == b
    | .float_ a, .float_ b => a == b
    | .imaginary a, .imaginary b => a == b
    | .string a, .string b => a == b
    | .bytes a, .bytes b => a == b
    | .fstringToken a, .fstringToken b => a == b
    | .keyword a, .keyword b => a == b
    | .operator a, .operator b => a == b
    | .delimiter a, .delimiter b => a == b
    | .indent, .indent => true
    | .dedent, .dedent => true
    | .newline, .newline => true
    | .nlToken, .nlToken => true
    | .endmarker, .endmarker => true
    | .comment a, .comment b => a == b
    | _, _ => false

instance : Inhabited TokenKind := ⟨.endmarker⟩

/-- A Python token with kind and source span. -/
structure Token where
  kind : TokenKind
  span : SourceSpan
  deriving Repr, BEq, Inhabited

end LeanPython.Lexer
