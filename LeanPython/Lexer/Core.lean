import LeanPython.Lexer.Types
import LeanPython.Lexer.Keywords
import LeanPython.Lexer.Operators
import LeanPython.Lexer.State
import LeanPython.Lexer.Char
import LeanPython.Lexer.Indent
import LeanPython.Lexer.Number
import LeanPython.Lexer.StringLit

set_option autoImplicit false

namespace LeanPython.Lexer

private def mkSpan (start stop : SourcePos) : SourceSpan := { start, stop }

/-- Create a token from kind and start position, using current pos as stop. -/
private def mkToken (kind : TokenKind) (start : SourcePos) : LexerM Token := do
  let stop ← LexerM.currentPos
  return { kind, span := mkSpan start stop }

-- ============================================================
-- Whitespace and continuation
-- ============================================================

/-- Skip horizontal whitespace (spaces, tabs, form feed). -/
private partial def skipWhitespace : LexerM Unit := do
  match ← LexerM.peekChar with
  | some c =>
    if isHorizWhitespace c then
      LexerM.advance
      skipWhitespace
    else
      pure ()
  | none => pure ()

/-- Skip whitespace and explicit line continuations (\<newline>). -/
private partial def skipWhitespaceAndContinuations : LexerM Unit := do
  skipWhitespace
  match ← LexerM.peekChar with
  | some '\\' =>
    -- Check if followed by newline
    let s ← get
    let nextPos := String.Pos.Raw.next s.source s.pos
    if nextPos.byteIdx < s.source.utf8ByteSize then
      let nextC := String.Pos.Raw.get s.source nextPos
      if nextC == '\n' then
        LexerM.advance  -- skip \
        LexerM.advance  -- skip \n
        skipWhitespaceAndContinuations
      else if nextC == '\r' then
        LexerM.advance  -- skip \
        LexerM.advance  -- skip \r
        -- Handle \r\n
        match ← LexerM.peekChar with
        | some '\n' => LexerM.advance
        | _ => pure ()
        skipWhitespaceAndContinuations
      else
        pure ()
    else
      pure ()
  | _ => pure ()

-- ============================================================
-- Newlines and comments
-- ============================================================

/-- Lex a newline token (\n or \r\n). -/
private def lexNewline (start : SourcePos) : LexerM Token := do
  match ← LexerM.nextChar with
  | some '\r' =>
    match ← LexerM.peekChar with
    | some '\n' => LexerM.advance
    | _ => pure ()
  | _ => pure ()
  let s ← get
  let kind := if s.bracketDepth > 0 then TokenKind.nlToken else TokenKind.newline
  mkToken kind start

/-- Read until end of line (for comments). -/
private partial def readToEol : LexerM Unit := do
  match ← LexerM.peekChar with
  | some c =>
    if c != '\n' && c != '\r' then
      LexerM.advance
      readToEol
    else
      pure ()
  | none => pure ()

/-- Lex a comment (# to end of line). -/
private def lexComment (start : SourcePos) (startPos : String.Pos.Raw) : LexerM Token := do
  readToEol
  let text ← LexerM.sliceFrom startPos
  mkToken (.comment text) start

-- ============================================================
-- Identifiers and keywords
-- ============================================================

/-- Read identifier continuation characters. -/
private partial def readIdentCont : LexerM Unit := do
  match ← LexerM.peekChar with
  | some c =>
    if isIdentCont c then
      LexerM.advance
      readIdentCont
    else
      pure ()
  | none => pure ()

/-- Lex an identifier or keyword. -/
private def lexIdentOrKeyword (start : SourcePos) (startPos : String.Pos.Raw) : LexerM Token := do
  LexerM.advance  -- consume first char (already checked)
  readIdentCont
  let name ← LexerM.sliceFrom startPos
  let kind := match Keyword.ofString? name with
    | some kw => TokenKind.keyword kw
    | none => TokenKind.name name
  mkToken kind start

-- ============================================================
-- Operators and delimiters
-- ============================================================

/-- Lex an operator or delimiter starting with character c. -/
private def lexOperatorOrDelimiter (start : SourcePos) (c : Char) : LexerM Token := do
  LexerM.advance  -- consume first char
  match c with
  | '+' =>
    match ← LexerM.peekChar with
    | some '=' => LexerM.advance; mkToken (.operator .plusEqual) start
    | _ => mkToken (.operator .plus) start
  | '-' =>
    match ← LexerM.peekChar with
    | some '=' => LexerM.advance; mkToken (.operator .minEqual) start
    | some '>' => LexerM.advance; mkToken (.operator .rarrow) start
    | _ => mkToken (.operator .minus) start
  | '*' =>
    match ← LexerM.peekChar with
    | some '*' =>
      LexerM.advance
      match ← LexerM.peekChar with
      | some '=' => LexerM.advance; mkToken (.operator .doubleStarEqual) start
      | _ => mkToken (.operator .doubleStar) start
    | some '=' => LexerM.advance; mkToken (.operator .starEqual) start
    | _ => mkToken (.operator .star) start
  | '/' =>
    match ← LexerM.peekChar with
    | some '/' =>
      LexerM.advance
      match ← LexerM.peekChar with
      | some '=' => LexerM.advance; mkToken (.operator .doubleSlashEqual) start
      | _ => mkToken (.operator .doubleSlash) start
    | some '=' => LexerM.advance; mkToken (.operator .slashEqual) start
    | _ => mkToken (.operator .slash) start
  | '%' =>
    match ← LexerM.peekChar with
    | some '=' => LexerM.advance; mkToken (.operator .percentEqual) start
    | _ => mkToken (.operator .percent) start
  | '@' =>
    match ← LexerM.peekChar with
    | some '=' => LexerM.advance; mkToken (.operator .atEqual) start
    | _ => mkToken (.operator .at) start
  | '&' =>
    match ← LexerM.peekChar with
    | some '=' => LexerM.advance; mkToken (.operator .amperEqual) start
    | _ => mkToken (.operator .amper) start
  | '|' =>
    match ← LexerM.peekChar with
    | some '=' => LexerM.advance; mkToken (.operator .vbarEqual) start
    | _ => mkToken (.operator .vbar) start
  | '^' =>
    match ← LexerM.peekChar with
    | some '=' => LexerM.advance; mkToken (.operator .circumflexEqual) start
    | _ => mkToken (.operator .circumflex) start
  | '~' => mkToken (.operator .tilde) start
  | '<' =>
    match ← LexerM.peekChar with
    | some '<' =>
      LexerM.advance
      match ← LexerM.peekChar with
      | some '=' => LexerM.advance; mkToken (.operator .leftShiftEqual) start
      | _ => mkToken (.operator .leftShift) start
    | some '=' => LexerM.advance; mkToken (.operator .lessEqual) start
    | _ => mkToken (.operator .less) start
  | '>' =>
    match ← LexerM.peekChar with
    | some '>' =>
      LexerM.advance
      match ← LexerM.peekChar with
      | some '=' => LexerM.advance; mkToken (.operator .rightShiftEqual) start
      | _ => mkToken (.operator .rightShift) start
    | some '=' => LexerM.advance; mkToken (.operator .greaterEqual) start
    | _ => mkToken (.operator .greater) start
  | '=' =>
    match ← LexerM.peekChar with
    | some '=' => LexerM.advance; mkToken (.operator .eqEqual) start
    | _ => mkToken (.operator .equal) start
  | '!' =>
    match ← LexerM.peekChar with
    | some '=' => LexerM.advance; mkToken (.operator .notEqual) start
    | _ => throw (LexError.unexpectedChar '!' start)
  | ':' =>
    match ← LexerM.peekChar with
    | some '=' => LexerM.advance; mkToken (.operator .colonEqual) start
    | _ => mkToken (.delimiter .colon) start
  | '.' =>
    match ← LexerM.peekChar with
    | some '.' =>
      -- Check for ellipsis (...)
      let s ← get
      let pos3 := String.Pos.Raw.next s.source s.pos
      if pos3.byteIdx < s.source.utf8ByteSize && String.Pos.Raw.get s.source pos3 == '.' then
        LexerM.advance  -- second .
        LexerM.advance  -- third .
        mkToken (.delimiter .ellipsis) start
      else
        mkToken (.delimiter .dot) start
    | _ => mkToken (.delimiter .dot) start
  | '(' =>
    modify fun s => { s with bracketDepth := s.bracketDepth + 1 }
    mkToken (.delimiter .lpar) start
  | ')' =>
    modify fun s => { s with bracketDepth := if s.bracketDepth > 0 then s.bracketDepth - 1 else 0 }
    mkToken (.delimiter .rpar) start
  | '[' =>
    modify fun s => { s with bracketDepth := s.bracketDepth + 1 }
    mkToken (.delimiter .lsqb) start
  | ']' =>
    modify fun s => { s with bracketDepth := if s.bracketDepth > 0 then s.bracketDepth - 1 else 0 }
    mkToken (.delimiter .rsqb) start
  | '{' =>
    modify fun s => { s with bracketDepth := s.bracketDepth + 1 }
    mkToken (.delimiter .lbrace) start
  | '}' =>
    modify fun s => { s with bracketDepth := if s.bracketDepth > 0 then s.bracketDepth - 1 else 0 }
    mkToken (.delimiter .rbrace) start
  | ',' => mkToken (.delimiter .comma) start
  | ';' => mkToken (.delimiter .semi) start
  | _ => throw (LexError.unexpectedChar c start)

-- ============================================================
-- String prefix detection
-- ============================================================

/-- Is this character a potential string prefix (b, B, r, R, f, F, u, U)? -/
private def isStringPrefix (c : Char) : Bool :=
  c == 'b' || c == 'B' || c == 'r' || c == 'R' ||
  c == 'f' || c == 'F' || c == 'u' || c == 'U'

/-- Check if starting at a string prefix char, this is actually a string literal.
    Look ahead to find if there's a quote after the prefix characters. -/
private def lexStringOrIdent (start : SourcePos) (startPos : String.Pos.Raw) (_c : Char) : LexerM Token := do
  -- Save state to potentially backtrack
  let savedState ← get
  -- Try to see if this is a string by peeking ahead through prefix chars
  let isStr ← checkIsString
  if isStr then
    -- Reset to start and let lexString handle it
    set savedState
    lexString start
  else
    -- It's an identifier
    set savedState
    lexIdentOrKeyword start startPos
where
  checkIsString : LexerM Bool := do
    -- Check sequences: b", r", f", u", rb", br", fr", rf", etc.
    match ← LexerM.peekAhead 1 with
    | some '\'' | some '"' =>
      -- Single prefix char followed by quote
      return true
    | some c2 =>
      if isStringPrefix c2 then
        -- Two prefix chars - check if followed by quote
        match ← LexerM.peekAhead 2 with
        | some '\'' | some '"' => return true
        | _ => return false
      else return false
    | none => return false

-- ============================================================
-- End of input
-- ============================================================

/-- Handle end-of-input: emit synthetic NEWLINE, remaining DEDENTs, then ENDMARKER. -/
private def endOfInput (pos : SourcePos) : LexerM Token := do
  let s ← get
  -- Build list of remaining tokens
  let mut remaining : List Token := []
  -- Emit remaining DEDENT tokens for indent levels > 0
  let mut stack := s.indentStack
  while decide (stack.length > 1) do
    match stack with
    | _ :: rest =>
      remaining := remaining ++ [{ kind := .dedent, span := mkSpan pos pos }]
      stack := rest
    | [] => break  -- shouldn't happen
  -- Always end with ENDMARKER
  remaining := remaining ++ [{ kind := .endmarker, span := mkSpan pos pos }]
  modify fun s => { s with
    pendingTokens := s.pendingTokens ++ remaining
    indentStack := [0]
  }
  if !s.atLineStart then
    return { kind := .newline, span := mkSpan pos pos }
  else
    -- Dequeue first pending token
    let s2 ← get
    match s2.pendingTokens with
    | tok :: rest =>
      set { s2 with pendingTokens := rest }
      return tok
    | [] => return { kind := .endmarker, span := mkSpan pos pos }

-- ============================================================
-- Main tokenization
-- ============================================================

/-- Lex a single token from the source. -/
partial def lexToken : LexerM Token := do
  -- Check pending tokens first
  let s ← get
  match s.pendingTokens with
  | tok :: rest =>
    set { s with pendingTokens := rest }
    return tok
  | [] => pure ()
  -- Process indentation at line start
  let s2 ← get
  if s2.atLineStart then
    let hasContent ← processIndent
    if hasContent then
      modify fun s => { s with atLineStart := false }
      -- Check if processIndent queued any tokens
      let s3 ← get
      match s3.pendingTokens with
      | tok :: rest =>
        set { s3 with pendingTokens := rest }
        return tok
      | [] => pure ()
  -- Skip whitespace and line continuations
  skipWhitespaceAndContinuations
  -- Get position
  let start ← LexerM.currentPos
  let s ← get
  let startPos := s.pos
  -- Check end of input
  if s.pos.byteIdx ≥ s.source.utf8ByteSize then
    return ← endOfInput start
  -- Dispatch on current character
  let c := String.Pos.Raw.get s.source s.pos
  let tok ←
    if c == '\n' || c == '\r' then
      lexNewline start
    else if c == '#' then
      lexComment start startPos
    else if c == '\'' || c == '"' then
      lexString start
    else if isStringPrefix c then
      lexStringOrIdent start startPos c
    else if isIdentStart c then
      lexIdentOrKeyword start startPos
    else if c.isDigit then
      lexNumber start startPos
    else
      lexOperatorOrDelimiter start c
  -- Update atLineStart based on token kind
  match tok.kind with
  | .newline | .nlToken => modify fun s => { s with atLineStart := true }
  | .indent | .dedent | .endmarker => pure ()
  | _ => modify fun s => { s with atLineStart := false }
  return tok

/-- Lex all tokens from the source until ENDMARKER. -/
partial def lexAll (acc : Array Token := #[]) : LexerM (Array Token) := do
  let tok ← lexToken
  let acc := acc.push tok
  match tok.kind with
  | .endmarker => return acc
  | _ => lexAll acc

end LeanPython.Lexer
