set_option autoImplicit false

namespace LeanPython.Lexer

/-- Python operator tokens. -/
inductive Operator where
  -- Arithmetic
  | plus              -- +
  | minus             -- -
  | star              -- *
  | slash             -- /
  | doubleSlash       -- //
  | percent           -- %
  | doubleStar        -- **
  | at                -- @
  -- Bitwise
  | amper             -- &
  | vbar              -- |
  | circumflex        -- ^
  | tilde             -- ~
  | leftShift         -- <<
  | rightShift        -- >>
  -- Comparison
  | eqEqual           -- ==
  | notEqual          -- !=
  | less              -- <
  | greater           -- >
  | lessEqual         -- <=
  | greaterEqual      -- >=
  -- Assignment
  | equal             -- =
  -- Augmented assignment
  | plusEqual          -- +=
  | minEqual          -- -=
  | starEqual         -- *=
  | slashEqual        -- /=
  | doubleSlashEqual  -- //=
  | percentEqual      -- %=
  | doubleStarEqual   -- **=
  | atEqual           -- @=
  | amperEqual        -- &=
  | vbarEqual         -- |=
  | circumflexEqual   -- ^=
  | leftShiftEqual    -- <<=
  | rightShiftEqual   -- >>=
  -- Special
  | rarrow            -- ->
  | colonEqual        -- :=
  deriving Repr, BEq, Inhabited

/-- Python delimiter tokens. -/
inductive Delimiter where
  | lpar     -- (
  | rpar     -- )
  | lsqb     -- [
  | rsqb     -- ]
  | lbrace   -- {
  | rbrace   -- }
  | colon    -- :
  | comma    -- ,
  | semi     -- ;
  | dot      -- .
  | ellipsis -- ...
  deriving Repr, BEq, Inhabited

end LeanPython.Lexer
