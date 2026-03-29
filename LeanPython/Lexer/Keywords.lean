set_option autoImplicit false

namespace LeanPython.Lexer

/-- Python 3.12 hard keywords. -/
inductive Keyword where
  | false_
  | none_
  | true_
  | and_
  | as_
  | assert_
  | async
  | await
  | break_
  | class_
  | continue_
  | def_
  | del
  | elif
  | else_
  | except_
  | finally_
  | for_
  | from_
  | global
  | if_
  | import_
  | in_
  | is_
  | lambda_
  | nonlocal
  | not_
  | or_
  | pass_
  | raise
  | return_
  | try_
  | while_
  | with_
  | yield_
  deriving Repr, BEq, Inhabited

namespace Keyword

/-- Look up a keyword from its Python source string. -/
def ofString? (s : String) : Option Keyword :=
  match s with
  | "False"    => some .false_
  | "None"     => some .none_
  | "True"     => some .true_
  | "and"      => some .and_
  | "as"       => some .as_
  | "assert"   => some .assert_
  | "async"    => some .async
  | "await"    => some .await
  | "break"    => some .break_
  | "class"    => some .class_
  | "continue" => some .continue_
  | "def"      => some .def_
  | "del"      => some .del
  | "elif"     => some .elif
  | "else"     => some .else_
  | "except"   => some .except_
  | "finally"  => some .finally_
  | "for"      => some .for_
  | "from"     => some .from_
  | "global"   => some .global
  | "if"       => some .if_
  | "import"   => some .import_
  | "in"       => some .in_
  | "is"       => some .is_
  | "lambda"   => some .lambda_
  | "nonlocal" => some .nonlocal
  | "not"      => some .not_
  | "or"       => some .or_
  | "pass"     => some .pass_
  | "raise"    => some .raise
  | "return"   => some .return_
  | "try"      => some .try_
  | "while"    => some .while_
  | "with"     => some .with_
  | "yield"    => some .yield_
  | _          => none

end Keyword

end LeanPython.Lexer
