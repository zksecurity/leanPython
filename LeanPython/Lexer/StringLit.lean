import LeanPython.Lexer.Types
import LeanPython.Lexer.State
import LeanPython.Lexer.Char

set_option autoImplicit false

namespace LeanPython.Lexer

/-- String prefix flags. -/
structure StrPrefix where
  raw   : Bool := false
  bytes : Bool := false
  fstr  : Bool := false
  deriving Repr, BEq

private def mkSpan (start stop : SourcePos) : SourceSpan := { start, stop }

private def mkToken (kind : TokenKind) (start : SourcePos) : LexerM Token := do
  let stop ← LexerM.currentPos
  return { kind, span := mkSpan start stop }

/-- Read string prefix characters (r, b, f, u and combinations). Returns prefix and whether any prefix was consumed. -/
private partial def readStringPrefix (pfx : StrPrefix := {}) : LexerM StrPrefix := do
  match ← LexerM.peekChar with
  | some 'r' | some 'R' =>
    if !pfx.raw then LexerM.advance; readStringPrefix { pfx with raw := true }
    else return pfx
  | some 'b' | some 'B' =>
    if !pfx.bytes then LexerM.advance; readStringPrefix { pfx with bytes := true }
    else return pfx
  | some 'f' | some 'F' =>
    if !pfx.fstr then LexerM.advance; readStringPrefix { pfx with fstr := true }
    else return pfx
  | some 'u' | some 'U' =>
    LexerM.advance; readStringPrefix pfx
  | _ => return pfx

/-- Process a single escape sequence, returning the unescaped character. -/
private def processEscape : LexerM Char := do
  match ← LexerM.nextChar with
  | some '\\' => return '\\'
  | some '\'' => return '\''
  | some '"'  => return '"'
  | some 'n'  => return '\n'
  | some 'r'  => return '\r'
  | some 't'  => return '\t'
  | some 'a'  => return '\x07'  -- bell
  | some 'b'  => return '\x08'  -- backspace
  | some 'f'  => return '\x0C'  -- form feed
  | some 'v'  => return '\x0B'  -- vertical tab
  | some '0'  => return '\x00'  -- null
  | some 'x' =>
    -- \xHH - two hex digits
    let d1 ← readHexDigit
    let d2 ← readHexDigit
    return Char.ofNat (d1 * 16 + d2)
  | some 'u' =>
    -- \uHHHH - four hex digits
    let d1 ← readHexDigit
    let d2 ← readHexDigit
    let d3 ← readHexDigit
    let d4 ← readHexDigit
    return Char.ofNat (d1 * 4096 + d2 * 256 + d3 * 16 + d4)
  | some c =>
    -- Unknown escape: in Python, \<other> is kept as-is
    return c
  | none =>
    let pos ← LexerM.currentPos
    throw (LexError.unexpectedEOF pos)
where
  readHexDigit : LexerM Nat := do
    match ← LexerM.nextChar with
    | some c =>
      if c.isDigit then return c.toNat - '0'.toNat
      else if c.val ≥ 0x61 && c.val ≤ 0x66 then return c.toNat - 'a'.toNat + 10
      else if c.val ≥ 0x41 && c.val ≤ 0x46 then return c.toNat - 'A'.toNat + 10
      else
        let pos ← LexerM.currentPos
        throw (LexError.invalidEscape c pos)
    | none =>
      let pos ← LexerM.currentPos
      throw (LexError.unexpectedEOF pos)

/-- Read string contents until the closing quote, processing escapes. -/
private partial def readStringBody (quote : Char) (triple : Bool) (raw : Bool)
    (acc : String) : LexerM String := do
  match ← LexerM.peekChar with
  | none =>
    let pos ← LexerM.currentPos
    throw (LexError.unterminatedString pos)
  | some c =>
    if c == quote then
      if triple then
        -- Need three consecutive quotes to close
        match ← LexerM.peekAhead 1 with
        | some c2 =>
          if c2 == quote then
            match ← LexerM.peekAhead 2 with
            | some c3 =>
              if c3 == quote then
                LexerM.advance; LexerM.advance; LexerM.advance  -- consume closing quotes
                return acc
              else
                LexerM.advance
                readStringBody quote triple raw (acc.push c)
            | none =>
              LexerM.advance
              readStringBody quote triple raw (acc.push c)
          else
            LexerM.advance
            readStringBody quote triple raw (acc.push c)
        | none =>
          LexerM.advance
          readStringBody quote triple raw (acc.push c)
      else
        LexerM.advance  -- consume closing quote
        return acc
    else if c == '\\' && !raw then
      LexerM.advance  -- consume backslash
      let escaped ← processEscape
      readStringBody quote triple raw (acc.push escaped)
    else if c == '\\' && raw then
      LexerM.advance
      -- In raw strings, backslash is literal but check for escaped quote
      match ← LexerM.peekChar with
      | some c2 =>
        if c2 == quote then
          -- Raw string: \' or \" keeps both the backslash and quote
          -- Actually in Python raw strings, \' still includes both chars
          -- but the string continues (the quote doesn't end it)
          LexerM.advance
          readStringBody quote triple raw ((acc.push '\\').push c2)
        else
          readStringBody quote triple raw (acc.push '\\')
      | none =>
        readStringBody quote triple raw (acc.push '\\')
    else if c == '\n' && !triple then
      let pos ← LexerM.currentPos
      throw (LexError.unterminatedString pos)
    else
      LexerM.advance
      readStringBody quote triple raw (acc.push c)

/-- Read bytes string body, similar to readStringBody but produces ByteArray. -/
private partial def readBytesBody (quote : Char) (triple : Bool) (raw : Bool)
    (acc : ByteArray) : LexerM ByteArray := do
  match ← LexerM.peekChar with
  | none =>
    let pos ← LexerM.currentPos
    throw (LexError.unterminatedString pos)
  | some c =>
    if c == quote then
      if triple then
        match ← LexerM.peekAhead 1 with
        | some c2 =>
          if c2 == quote then
            match ← LexerM.peekAhead 2 with
            | some c3 =>
              if c3 == quote then
                LexerM.advance; LexerM.advance; LexerM.advance
                return acc
              else
                LexerM.advance
                readBytesBody quote triple raw (acc.push c.toUInt8)
            | none =>
              LexerM.advance
              readBytesBody quote triple raw (acc.push c.toUInt8)
          else
            LexerM.advance
            readBytesBody quote triple raw (acc.push c.toUInt8)
        | none =>
          LexerM.advance
          readBytesBody quote triple raw (acc.push c.toUInt8)
      else
        LexerM.advance
        return acc
    else if c == '\\' && !raw then
      LexerM.advance
      let escaped ← processEscape
      readBytesBody quote triple raw (acc.push escaped.toUInt8)
    else if c == '\n' && !triple then
      let pos ← LexerM.currentPos
      throw (LexError.unterminatedString pos)
    else
      LexerM.advance
      readBytesBody quote triple raw (acc.push c.toUInt8)

/-- Lex a string or bytes literal. Called when current position is at a string prefix
    character or quote character. -/
partial def lexString (start : SourcePos) : LexerM Token := do
  -- Read prefix
  let pfx ← readStringPrefix
  -- Read opening quote
  match ← LexerM.nextChar with
  | some q =>
    if q != '\'' && q != '"' then
      let pos ← LexerM.currentPos
      throw (LexError.unexpectedChar q pos)
    -- Check for triple quote
    let triple ← do
      match ← LexerM.peekChar with
      | some c =>
        if c == q then
          match ← LexerM.peekAhead 1 with
          | some c2 =>
            if c2 == q then
              LexerM.advance; LexerM.advance
              pure true
            else pure false
          | none => pure false
        else pure false
      | none => pure false
    -- F-strings: emit fstringStart token
    if pfx.fstr then
      mkToken .fstringStart start
    -- Bytes strings
    else if pfx.bytes then
      let body ← readBytesBody q triple pfx.raw ByteArray.empty
      mkToken (.bytes body) start
    -- Regular strings
    else
      let body ← readStringBody q triple pfx.raw ""
      mkToken (.string body) start
  | none =>
    let pos ← LexerM.currentPos
    throw (LexError.unexpectedEOF pos)

end LeanPython.Lexer
