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

/-- Read a format spec after : in an f-string expression. Consumes up to }. -/
private partial def readFStringFormatSpec (acc : String) : LexerM String := do
  match ← LexerM.peekChar with
  | none =>
    let pos ← LexerM.currentPos
    throw (LexError.unterminatedString pos)
  | some '}' =>
    LexerM.advance  -- consume }
    return acc
  | some c =>
    LexerM.advance
    readFStringFormatSpec (acc.push c)

/-- Read a string literal inside an f-string expression (e.g. f"{'hello'}"). -/
private partial def readFStringInnerString (quote : Char) (acc : String) : LexerM String := do
  match ← LexerM.peekChar with
  | none =>
    let pos ← LexerM.currentPos
    throw (LexError.unterminatedString pos)
  | some c =>
    if c == quote then
      LexerM.advance
      return acc.push c
    else if c == '\\' then
      LexerM.advance
      match ← LexerM.peekChar with
      | some esc => LexerM.advance; readFStringInnerString quote ((acc.push '\\').push esc)
      | none => let pos ← LexerM.currentPos; throw (LexError.unterminatedString pos)
    else
      LexerM.advance
      readFStringInnerString quote (acc.push c)

/-- Read the expression part of an f-string `{...}`, handling nested braces.
    Returns the expression source and optional conversion character. -/
private partial def readFStringExpr (depth : Nat) (acc : String) : LexerM (String × Option Char) := do
  match ← LexerM.peekChar with
  | none =>
    let pos ← LexerM.currentPos
    throw (LexError.unterminatedString pos)
  | some '}' =>
    if depth == 0 then
      LexerM.advance  -- consume closing }
      return (acc, none)
    else
      LexerM.advance
      readFStringExpr (depth - 1) (acc.push '}')
  | some '{' =>
    LexerM.advance
    readFStringExpr (depth + 1) (acc.push '{')
  | some '!' =>
    -- Check if this is a conversion flag (!r, !s, !a) before }
    match ← LexerM.peekAhead 1 with
    | some convChar =>
      if (convChar == 'r' || convChar == 's' || convChar == 'a') && depth == 0 then
        -- Check if next char after conversion is } or :
        match ← LexerM.peekAhead 2 with
        | some '}' =>
          LexerM.advance  -- consume !
          LexerM.advance  -- consume conversion char
          LexerM.advance  -- consume }
          return (acc, some convChar)
        | some ':' =>
          -- Has format spec after conversion - skip format spec for now
          LexerM.advance  -- consume !
          LexerM.advance  -- consume conversion char
          let _formatSpec ← readFStringFormatSpec ""
          return (acc, some convChar)
        | _ =>
          -- Not a conversion, treat ! as part of expression
          LexerM.advance
          readFStringExpr depth (acc.push '!')
      else
        LexerM.advance
        readFStringExpr depth (acc.push '!')
    | none =>
      let pos ← LexerM.currentPos
      throw (LexError.unterminatedString pos)
  | some ':' =>
    if depth == 0 then
      -- Format spec - skip it for now
      LexerM.advance  -- consume :
      let _formatSpec ← readFStringFormatSpec ""
      return (acc, none)
    else
      LexerM.advance
      readFStringExpr depth (acc.push ':')
  | some '\'' | some '"' =>
    -- String literal inside expression - read until matching quote
    let q := (← LexerM.peekChar).get!
    LexerM.advance
    let inner ← readFStringInnerString q (acc.push q)
    readFStringExpr depth inner
  | some c =>
    LexerM.advance
    readFStringExpr depth (acc.push c)

/-- Read the body of an f-string, producing an array of FStringParts.
    Handles literal text and `{expr}` / `{expr!r}` / `{expr:spec}` expressions. -/
private partial def readFStringBody (quote : Char) (triple : Bool) (_raw : Bool)
    (parts : Array FStringPart) (litAcc : String) : LexerM (Array FStringPart) := do
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
                let parts := if litAcc.isEmpty then parts else parts.push (.literal litAcc)
                return parts
              else
                LexerM.advance
                readFStringBody quote triple _raw parts (litAcc.push c)
            | none =>
              LexerM.advance
              readFStringBody quote triple _raw parts (litAcc.push c)
          else
            LexerM.advance
            readFStringBody quote triple _raw parts (litAcc.push c)
        | none =>
          LexerM.advance
          readFStringBody quote triple _raw parts (litAcc.push c)
      else
        LexerM.advance  -- consume closing quote
        let parts := if litAcc.isEmpty then parts else parts.push (.literal litAcc)
        return parts
    else if c == '{' then
      -- Check for {{ (escaped brace → literal {)
      match ← LexerM.peekAhead 1 with
      | some '{' =>
        LexerM.advance; LexerM.advance
        readFStringBody quote triple _raw parts (litAcc.push '{')
      | _ =>
        LexerM.advance  -- consume {
        -- Flush any accumulated literal text
        let parts := if litAcc.isEmpty then parts else parts.push (.literal litAcc)
        -- Read expression
        let (exprSrc, conv) ← readFStringExpr 0 ""
        let parts := parts.push (.expr exprSrc conv)
        readFStringBody quote triple _raw parts ""
    else if c == '}' then
      -- Check for }} (escaped brace → literal })
      match ← LexerM.peekAhead 1 with
      | some '}' =>
        LexerM.advance; LexerM.advance
        readFStringBody quote triple _raw parts (litAcc.push '}')
      | _ =>
        LexerM.advance
        readFStringBody quote triple _raw parts (litAcc.push '}')
    else if c == '\\' then
      LexerM.advance
      let escaped ← processEscape
      readFStringBody quote triple _raw parts (litAcc.push escaped)
    else if c == '\n' && !triple then
      let pos ← LexerM.currentPos
      throw (LexError.unterminatedString pos)
    else
      LexerM.advance
      readFStringBody quote triple _raw parts (litAcc.push c)

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
    -- F-strings: read body and produce fstringToken
    if pfx.fstr then
      let parts ← readFStringBody q triple pfx.raw #[] ""
      mkToken (.fstringToken parts) start
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
