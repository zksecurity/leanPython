import Lython.Lexer.Types
import Lython.Lexer.State
import Lython.Lexer.Char

set_option autoImplicit false

namespace Lython.Lexer

private def mkSpan (start stop : SourcePos) : SourceSpan := { start, stop }

/-- Count leading whitespace on the current line. Tabs expand to the next
    multiple of 8. Returns the indent level (column number). -/
private partial def measureIndent (col : Nat := 0) : LexerM Nat := do
  match ← LexerM.peekChar with
  | some ' ' =>
    LexerM.advance
    measureIndent (col + 1)
  | some '\t' =>
    LexerM.advance
    -- Tabs expand to the next multiple of 8
    measureIndent ((col / 8 + 1) * 8)
  | some '\x0C' =>
    -- Form feed resets column to 0
    LexerM.advance
    measureIndent 0
  | _ => return col

/-- Check if the rest of the current line is blank (only whitespace/comments). -/
private partial def isBlankLine : LexerM Bool := do
  match ← LexerM.peekChar with
  | some '\n' | some '\r' | none => return true
  | some '#' => return true
  | _ => return false

/-- Generate DEDENT tokens to reach the target indent level.
    Pops from indent stack and generates one DEDENT per pop. -/
private def generateDedents (targetLevel : Nat) (pos : SourcePos) : LexerM (List Token) := do
  let s ← get
  let mut stack := s.indentStack
  let mut tokens : List Token := []
  while decide (stack.head? != some targetLevel) do
    match stack with
    | [] => throw (LexError.inconsistentIndent pos)
    | top :: rest =>
      if top > targetLevel then
        tokens := { kind := .dedent, span := mkSpan pos pos } :: tokens
        stack := rest
      else if top < targetLevel then
        throw (LexError.inconsistentIndent pos)
      else
        -- top == targetLevel, done
        break
  modify fun s => { s with indentStack := stack }
  return tokens.reverse

/-- Process indentation at the start of a line. Called when atLineStart is true.
    May emit INDENT, DEDENT tokens, or skip blank/comment-only lines.
    Returns true if a non-blank content line was found. -/
partial def processIndent : LexerM Bool := do
  let s ← get
  -- Don't process indent inside brackets
  if s.bracketDepth > 0 then
    return true
  -- Measure the indent level
  let indentLevel ← measureIndent
  -- Check if line is blank
  let blank ← isBlankLine
  if blank then
    -- Blank/comment-only lines: don't change indent
    return false
  -- Compare with current indent
  let pos ← LexerM.currentPos
  let currentIndent := s.indentStack.head!
  if indentLevel > currentIndent then
    -- Indent: push new level
    modify fun s => { s with
      indentStack := indentLevel :: s.indentStack
      pendingTokens := s.pendingTokens ++ [{ kind := .indent, span := mkSpan pos pos }]
    }
  else if indentLevel < currentIndent then
    -- Dedent: pop levels
    let dedents ← generateDedents indentLevel pos
    modify fun s => { s with
      pendingTokens := s.pendingTokens ++ dedents
    }
  -- If equal, no tokens needed
  return true

end Lython.Lexer
