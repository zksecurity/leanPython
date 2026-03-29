import LeanPython.Lexer.Types
import LeanPython.Lexer.State
import LeanPython.Lexer.Char

set_option autoImplicit false

namespace LeanPython.Lexer

/-- Read decimal digits, accumulating value. Skips underscores. -/
private partial def readDecVal (acc : Nat) : LexerM Nat := do
  match ← LexerM.peekChar with
  | some c =>
    if c.isDigit then
      LexerM.advance
      readDecVal (acc * 10 + (c.toNat - '0'.toNat))
    else if c == '_' then
      LexerM.advance
      readDecVal acc
    else
      return acc
  | none => return acc

/-- Read decimal digits, returning (value, digitCount). -/
private partial def readDecValCount (acc : Nat) (count : Nat) : LexerM (Nat × Nat) := do
  match ← LexerM.peekChar with
  | some c =>
    if c.isDigit then
      LexerM.advance
      readDecValCount (acc * 10 + (c.toNat - '0'.toNat)) (count + 1)
    else if c == '_' then
      LexerM.advance
      readDecValCount acc count
    else
      return (acc, count)
  | none => return (acc, count)

/-- Read hex digits, accumulating value. -/
private partial def readHexVal (acc : Nat) : LexerM Nat := do
  match ← LexerM.peekChar with
  | some c =>
    if c == '_' then
      LexerM.advance
      readHexVal acc
    else
      match hexDigitVal c with
      | some d =>
        LexerM.advance
        readHexVal (acc * 16 + d)
      | none => return acc
  | none => return acc
where
  hexDigitVal (c : Char) : Option Nat :=
    if c.isDigit then some (c.toNat - '0'.toNat)
    else if c.val ≥ 0x61 && c.val ≤ 0x66 then some (c.toNat - 'a'.toNat + 10)
    else if c.val ≥ 0x41 && c.val ≤ 0x46 then some (c.toNat - 'A'.toNat + 10)
    else none

/-- Read octal digits, accumulating value. -/
private partial def readOctVal (acc : Nat) : LexerM Nat := do
  match ← LexerM.peekChar with
  | some c =>
    if c == '_' then
      LexerM.advance
      readOctVal acc
    else if isOctDigit c then
      LexerM.advance
      readOctVal (acc * 8 + (c.toNat - '0'.toNat))
    else
      return acc
  | none => return acc

/-- Read binary digits, accumulating value. -/
private partial def readBinVal (acc : Nat) : LexerM Nat := do
  match ← LexerM.peekChar with
  | some c =>
    if c == '_' then
      LexerM.advance
      readBinVal acc
    else if c == '0' then
      LexerM.advance
      readBinVal (acc * 2)
    else if c == '1' then
      LexerM.advance
      readBinVal (acc * 2 + 1)
    else
      return acc
  | none => return acc

private def mkSpan (start stop : SourcePos) : SourceSpan := { start, stop }

private def mkToken (kind : TokenKind) (start : SourcePos) : LexerM Token := do
  let stop ← LexerM.currentPos
  return { kind, span := mkSpan start stop }

/-- Lex a numeric literal (integer, float, or imaginary). -/
partial def lexNumber (start : SourcePos) (_startPos : String.Pos.Raw) : LexerM Token := do
  let s ← get
  let firstChar := String.Pos.Raw.get s.source s.pos
  LexerM.advance
  if firstChar == '0' then
    match ← LexerM.peekChar with
    | some 'x' | some 'X' =>
      LexerM.advance
      let n ← readHexVal 0
      mkToken (.integer n) start
    | some 'o' | some 'O' =>
      LexerM.advance
      let n ← readOctVal 0
      mkToken (.integer n) start
    | some 'b' | some 'B' =>
      LexerM.advance
      let n ← readBinVal 0
      mkToken (.integer n) start
    | _ => lexDecimalRest (firstChar.toNat - '0'.toNat) start
  else
    lexDecimalRest (firstChar.toNat - '0'.toNat) start
where
  lexDecimalRest (intSoFar : Nat) (start : SourcePos) : LexerM Token := do
    let intPart ← readDecVal intSoFar
    match ← LexerM.peekChar with
    | some '.' =>
      -- Check it's not '..' (range/ellipsis)
      match ← LexerM.peekAhead 1 with
      | some '.' => mkToken (.integer intPart) start
      | _ =>
        LexerM.advance  -- consume .
        let (fracVal, fracDigits) ← readDecValCount 0 0
        lexExponentFloat intPart fracVal fracDigits start
    | some 'e' | some 'E' => lexExponentFloat intPart 0 0 start
    | some 'j' | some 'J' =>
      LexerM.advance
      mkToken (.imaginary (Float.ofNat intPart)) start
    | _ => mkToken (.integer intPart) start

  lexExponentFloat (intPart fracVal fracDigits : Nat) (start : SourcePos) : LexerM Token := do
    let mut expVal : Int := 0
    match ← LexerM.peekChar with
    | some 'e' | some 'E' =>
      LexerM.advance
      let mut expSign : Int := 1
      match ← LexerM.peekChar with
      | some '+' => LexerM.advance
      | some '-' => LexerM.advance; expSign := -1
      | _ => pure ()
      let e ← readDecVal 0
      expVal := expSign * Int.ofNat e
    | _ => pure ()
    -- Compute float: (intPart * 10^fracDigits + fracVal) * 10^(expVal - fracDigits)
    let mantissa := intPart * Nat.pow 10 fracDigits + fracVal
    let netExp := expVal - Int.ofNat fracDigits
    let f := if netExp ≥ 0 then
      Float.ofScientific mantissa false netExp.toNat
    else
      Float.ofScientific mantissa true (-netExp).toNat
    match ← LexerM.peekChar with
    | some 'j' | some 'J' =>
      LexerM.advance
      mkToken (.imaginary f) start
    | _ => mkToken (.float_ f) start

end LeanPython.Lexer
