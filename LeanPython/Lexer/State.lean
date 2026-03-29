import LeanPython.Lexer.Types

set_option autoImplicit false

namespace LeanPython.Lexer

/-- Lexer state tracking position, indentation, and bracket nesting. -/
structure LexerState where
  source        : String
  pos           : String.Pos.Raw
  line          : Nat             -- 1-based
  col           : Nat             -- 0-based
  indentStack   : List Nat        -- indent levels; [0] initially
  bracketDepth  : Nat             -- nesting depth of ( [ {
  atLineStart   : Bool            -- true at beginning of a new line
  pendingTokens : List Token
  deriving Inhabited

/-- The lexer monad: state + exceptions. -/
abbrev LexerM := StateT LexerState (Except LexError)

/-- Create initial lexer state for a source string. -/
def LexerState.initial (source : String) : LexerState where
  source        := source
  pos           := ⟨0⟩
  line          := 1
  col           := 0
  indentStack   := [0]
  bracketDepth  := 0
  atLineStart   := true
  pendingTokens := []

namespace LexerM

/-- Get the current source position. -/
def currentPos : LexerM SourcePos := do
  let s ← get
  return { line := s.line, column := s.col, offset := s.pos.byteIdx }

/-- Check if we've reached end of source. -/
def atEnd : LexerM Bool := do
  let s ← get
  return decide (s.pos.byteIdx ≥ s.source.utf8ByteSize)

/-- Peek at the current character without consuming it. -/
def peekChar : LexerM (Option Char) := do
  let s ← get
  if s.pos.byteIdx < s.source.utf8ByteSize then
    return some (String.Pos.Raw.get s.source s.pos)
  else
    return none

/-- Advance n positions in the source string. -/
private def nthPos (source : String) (pos : String.Pos.Raw) : Nat → String.Pos.Raw
  | 0 => pos
  | n + 1 =>
    if pos.byteIdx < source.utf8ByteSize then
      nthPos source (String.Pos.Raw.next source pos) n
    else
      pos

/-- Peek at the character n positions ahead (0 = current). -/
def peekAhead (n : Nat) : LexerM (Option Char) := do
  let s ← get
  let p := nthPos s.source s.pos n
  if p.byteIdx < s.source.utf8ByteSize then
    return some (String.Pos.Raw.get s.source p)
  else
    return none

/-- Consume and return the current character. -/
def nextChar : LexerM (Option Char) := do
  let s ← get
  if s.pos.byteIdx < s.source.utf8ByteSize then
    let c := String.Pos.Raw.get s.source s.pos
    let nextPos := String.Pos.Raw.next s.source s.pos
    let newLine := if c == '\n' then s.line + 1 else s.line
    let newCol := if c == '\n' then 0 else s.col + 1
    set { s with pos := nextPos, line := newLine, col := newCol }
    return some c
  else
    return none

/-- Advance position by one character (discarding it). -/
def advance : LexerM Unit := do
  let _ ← nextChar

/-- Extract substring from the given position to current position. -/
def sliceFrom (startPos : String.Pos.Raw) : LexerM String := do
  let s ← get
  return (Substring.Raw.mk s.source startPos s.pos).toString

end LexerM

end LeanPython.Lexer
