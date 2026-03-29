import LeanPython.Parser.Types
import LeanPython.Lexer.Types

set_option autoImplicit false

namespace LeanPython.Parser

open LeanPython.Lexer (SourcePos SourceSpan Token TokenKind)

/-- A default source span for synthetic positions. -/
private def defaultSpan : SourceSpan :=
  { start := { line := 0, column := 0, offset := 0 },
    stop  := { line := 0, column := 0, offset := 0 } }

/-- Peek at the current token without consuming it. -/
def peek : ParserM (Option Token) := do
  let s ← get
  if h : s.pos < s.tokens.size then
    return some s.tokens[s.pos]
  else
    return none

/-- Peek at the kind of the current token. -/
def peekKind : ParserM (Option TokenKind) := do
  match ← peek with
  | some tok => return some tok.kind
  | none     => return none

/-- Advance past the current token and return it. -/
def advance : ParserM Token := do
  let s ← get
  if h : s.pos < s.tokens.size then
    let tok := s.tokens[s.pos]
    set { s with pos := s.pos + 1 }
    return tok
  else
    throw (.unexpectedEOF "advance past end of tokens")

/-- Check if the parser has reached the end of input. -/
def atEnd : ParserM Bool := do
  let s ← get
  return s.pos >= s.tokens.size

/-- Get the source span of the current token, or a default if at end. -/
def currentSpan : ParserM SourceSpan := do
  match ← peek with
  | some tok => return tok.span
  | none     => return defaultSpan

/-- Build a span from `start` to the previous token's end (current pos - 1). -/
def spanFrom (start : SourceSpan) : ParserM SourceSpan := do
  let s ← get
  if s.pos > 0 then
    if h : s.pos - 1 < s.tokens.size then
      let prev := s.tokens[s.pos - 1]
      return { start := start.start, stop := prev.span.stop }
    else
      return start
  else
    return start

/-- Try a parser. On failure, restore position and return `none`. -/
def attempt {α : Type} (p : ParserM α) : ParserM (Option α) := do
  let s ← get
  let savedPos := s.pos
  match p s with
  | .ok (a, s') => set s'; return some a
  | .error _ => modify fun st => { st with pos := savedPos }; return none

/-- Try first parser; on failure, try second (PEG ordered choice). -/
def orElse {α : Type} (p1 p2 : ParserM α) : ParserM α := do
  match ← attempt p1 with
  | some a => return a
  | none   => p2

/-- Optional: try parser, return `Option`. -/
def optional {α : Type} (p : ParserM α) : ParserM (Option α) := attempt p

/-- Zero or more repetitions. -/
partial def many {α : Type} (p : ParserM α) : ParserM (List α) := do
  match ← attempt p with
  | some a => return a :: (← many p)
  | none   => return []

/-- One or more repetitions. -/
def many1 {α : Type} (p : ParserM α) : ParserM (List α) := do
  let first ← p
  let rest ← many p
  return first :: rest

/-- Parse one or more items separated by a delimiter. -/
partial def sepBy1 {α : Type} (p : ParserM α) (sep : ParserM Unit) : ParserM (List α) := do
  let first ← p
  let mut items := [first]
  while (← attempt sep).isSome do
    items := items ++ [← p]
  return items

/-- Parse zero or more items separated by a delimiter. -/
def sepBy {α : Type} (p : ParserM α) (sep : ParserM Unit) : ParserM (List α) := do
  match ← attempt p with
  | some first =>
    let mut items := [first]
    while (← attempt sep).isSome do
      items := items ++ [← p]
    return items
  | none => return []

/-- Discard the result of a parser (useful for `advance` in expressions). -/
def discard {α : Type} (p : ParserM α) : ParserM Unit := do
  let _ ← p

end LeanPython.Parser
