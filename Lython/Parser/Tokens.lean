import Lython.Parser.Combinators
import Lython.Lexer.Keywords
import Lython.Lexer.Operators
import Lython.AST.Types

set_option autoImplicit false

namespace Lython.Parser

open Lython.Lexer (SourceSpan Token TokenKind Keyword Operator Delimiter)

/-- Expect a specific keyword token and consume it. -/
def expectKeyword (kw : Keyword) : ParserM Token := do
  match ← peek with
  | some tok =>
    match tok.kind with
    | .keyword k => if k == kw then discard advance; return tok
                    else throw (.expectedToken s!"keyword {repr kw}" tok.kind tok.span.start)
    | _ => throw (.expectedToken s!"keyword {repr kw}" tok.kind tok.span.start)
  | none => throw (.unexpectedEOF s!"expected keyword {repr kw}")

/-- Expect a specific operator token and consume it. -/
def expectOperator (op : Operator) : ParserM Token := do
  match ← peek with
  | some tok =>
    match tok.kind with
    | .operator o => if o == op then discard advance; return tok
                     else throw (.expectedToken s!"operator {repr op}" tok.kind tok.span.start)
    | _ => throw (.expectedToken s!"operator {repr op}" tok.kind tok.span.start)
  | none => throw (.unexpectedEOF s!"expected operator {repr op}")

/-- Expect a specific delimiter token and consume it. -/
def expectDelimiter (d : Delimiter) : ParserM Token := do
  match ← peek with
  | some tok =>
    match tok.kind with
    | .delimiter d' => if d' == d then discard advance; return tok
                       else throw (.expectedToken s!"delimiter {repr d}" tok.kind tok.span.start)
    | _ => throw (.expectedToken s!"delimiter {repr d}" tok.kind tok.span.start)
  | none => throw (.unexpectedEOF s!"expected delimiter {repr d}")

/-- Parse a NAME token, returning the name string and span. -/
def parseName : ParserM (String × SourceSpan) := do
  match ← peek with
  | some tok =>
    match tok.kind with
    | .name n => discard advance; return (n, tok.span)
    | _ => throw (.expectedToken "identifier" tok.kind tok.span.start)
  | none => throw (.unexpectedEOF "expected identifier")

/-- Parse an INTEGER token. -/
def parseInteger : ParserM (Int × SourceSpan) := do
  match ← peek with
  | some tok =>
    match tok.kind with
    | .integer n => discard advance; return (n, tok.span)
    | _ => throw (.expectedToken "integer" tok.kind tok.span.start)
  | none => throw (.unexpectedEOF "expected integer")

/-- Parse a FLOAT token. -/
def parseFloat : ParserM (Float × SourceSpan) := do
  match ← peek with
  | some tok =>
    match tok.kind with
    | .float_ f => discard advance; return (f, tok.span)
    | _ => throw (.expectedToken "float" tok.kind tok.span.start)
  | none => throw (.unexpectedEOF "expected float")

/-- Parse an IMAGINARY token. -/
def parseImaginary : ParserM (Float × SourceSpan) := do
  match ← peek with
  | some tok =>
    match tok.kind with
    | .imaginary f => discard advance; return (f, tok.span)
    | _ => throw (.expectedToken "imaginary" tok.kind tok.span.start)
  | none => throw (.unexpectedEOF "expected imaginary literal")

/-- Parse a STRING token. -/
def parseString : ParserM (String × SourceSpan) := do
  match ← peek with
  | some tok =>
    match tok.kind with
    | .string s => discard advance; return (s, tok.span)
    | _ => throw (.expectedToken "string" tok.kind tok.span.start)
  | none => throw (.unexpectedEOF "expected string")

/-- Parse a BYTES token. -/
def parseBytes : ParserM (ByteArray × SourceSpan) := do
  match ← peek with
  | some tok =>
    match tok.kind with
    | .bytes b => discard advance; return (b, tok.span)
    | _ => throw (.expectedToken "bytes" tok.kind tok.span.start)
  | none => throw (.unexpectedEOF "expected bytes literal")

/-- Expect a NEWLINE token and consume it. -/
def expectNewline : ParserM Token := do
  match ← peek with
  | some tok =>
    match tok.kind with
    | .newline => discard advance; return tok
    | _ => throw (.expectedToken "newline" tok.kind tok.span.start)
  | none => throw (.unexpectedEOF "expected newline")

/-- Expect an INDENT token and consume it. -/
def expectIndent : ParserM Token := do
  match ← peek with
  | some tok =>
    match tok.kind with
    | .indent => discard advance; return tok
    | _ => throw (.expectedToken "indent" tok.kind tok.span.start)
  | none => throw (.unexpectedEOF "expected indent")

/-- Expect a DEDENT token and consume it. -/
def expectDedent : ParserM Token := do
  match ← peek with
  | some tok =>
    match tok.kind with
    | .dedent => discard advance; return tok
    | _ => throw (.expectedToken "dedent" tok.kind tok.span.start)
  | none => throw (.unexpectedEOF "expected dedent")

/-- Check if the current token is a specific keyword (lookahead, no consume). -/
def isKeyword (kw : Keyword) : ParserM Bool := do
  match ← peekKind with
  | some (.keyword k) => return k == kw
  | _ => return false

/-- Check if the current token is a specific operator (lookahead). -/
def isOperator (op : Operator) : ParserM Bool := do
  match ← peekKind with
  | some (.operator o) => return o == op
  | _ => return false

/-- Check if the current token is a specific delimiter (lookahead). -/
def isDelimiter (d : Delimiter) : ParserM Bool := do
  match ← peekKind with
  | some (.delimiter d') => return d' == d
  | _ => return false

/-- Check if the current token is a NEWLINE (lookahead). -/
def isNewline : ParserM Bool := do
  match ← peekKind with
  | some .newline => return true
  | _ => return false

/-- Check if the current token is an INDENT (lookahead). -/
def isIndent : ParserM Bool := do
  match ← peekKind with
  | some .indent => return true
  | _ => return false

/-- Check if the current token is an ENDMARKER (lookahead). -/
def isEndmarker : ParserM Bool := do
  match ← peekKind with
  | some .endmarker => return true
  | _ => return false

/-- Check if the current token is a NAME (lookahead). -/
def isName : ParserM Bool := do
  match ← peekKind with
  | some (.name _) => return true
  | _ => return false

/-- Check if current token is any augmented assignment operator. Returns the
    corresponding `BinOp` if so. -/
def tryAugAssignOp : ParserM (Option Lython.AST.BinOp) := do
  match ← peekKind with
  | some (.operator .plusEqual)       => discard advance; return some .add
  | some (.operator .minEqual)        => discard advance; return some .sub
  | some (.operator .starEqual)       => discard advance; return some .mult
  | some (.operator .slashEqual)      => discard advance; return some .div
  | some (.operator .doubleSlashEqual) => discard advance; return some .floorDiv
  | some (.operator .percentEqual)    => discard advance; return some .mod
  | some (.operator .doubleStarEqual) => discard advance; return some .pow
  | some (.operator .atEqual)         => discard advance; return some .matMult
  | some (.operator .amperEqual)      => discard advance; return some .bitAnd
  | some (.operator .vbarEqual)       => discard advance; return some .bitOr
  | some (.operator .circumflexEqual) => discard advance; return some .bitXor
  | some (.operator .leftShiftEqual)  => discard advance; return some .lShift
  | some (.operator .rightShiftEqual) => discard advance; return some .rShift
  | _ => return none

end Lython.Parser
