set_option autoImplicit false

namespace LeanPython.Lexer

/-- Is the character a valid Python identifier start? (ASCII subset) -/
def isIdentStart (c : Char) : Bool :=
  c.isAlpha || c == '_'

/-- Is the character a valid Python identifier continuation? (ASCII subset) -/
def isIdentCont (c : Char) : Bool :=
  c.isAlphanum || c == '_'

/-- Is the character an octal digit? -/
def isOctDigit (c : Char) : Bool :=
  c.val ≥ 0x30 && c.val ≤ 0x37

/-- Is the character a binary digit? -/
def isBinDigit (c : Char) : Bool :=
  c == '0' || c == '1'

/-- Is the character a hex digit? -/
def isHexDigit (c : Char) : Bool :=
  c.isDigit || (c.val ≥ 0x61 && c.val ≤ 0x66) || (c.val ≥ 0x41 && c.val ≤ 0x46)

/-- Is the character horizontal whitespace (not newline)? -/
def isHorizWhitespace (c : Char) : Bool :=
  c == ' ' || c == '\t' || c == '\x0C'

end LeanPython.Lexer
