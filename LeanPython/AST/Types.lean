import LeanPython.Lexer.Types

set_option autoImplicit false

namespace LeanPython.AST

open LeanPython.Lexer (SourceSpan)

/-- Binary operators for `Expr.binOp`. -/
inductive BinOp where
  | add | sub | mult | matMult | div | mod | pow
  | lShift | rShift | bitOr | bitXor | bitAnd | floorDiv
  deriving Repr, BEq, Inhabited

/-- Unary operators for `Expr.unaryOp`. -/
inductive UnaryOp where
  | invert | not_ | uAdd | uSub
  deriving Repr, BEq, Inhabited

/-- Boolean operators for `Expr.boolOp`. -/
inductive BoolOp where
  | and_ | or_
  deriving Repr, BEq, Inhabited

/-- Comparison operators for `Expr.compare`. -/
inductive CmpOp where
  | eq | notEq | lt | ltE | gt | gtE | is_ | isNot | in_ | notIn
  deriving Repr, BEq, Inhabited

/-- Constant values (literals and singletons). -/
inductive Constant where
  | none_
  | true_
  | false_
  | ellipsis
  | int       : Int → Constant
  | float_    : Float → Constant
  | imaginary : Float → Constant
  | string    : String → Constant
  | bytes     : ByteArray → Constant
  deriving Repr, BEq, Inhabited

/-! ## Expression-level mutual block

`Expr`, `Comprehension`, `Arg`, `Arguments`, and `CallKeyword` are mutually
recursive because `Expr` contains `Comprehension`/`Arguments`/`CallKeyword` in
some constructors, and those types contain `Expr` in their fields.

Lean4 does not allow `structure` in `mutual` blocks, so record-like types use
`inductive ... where | mk : ...` with accessor functions defined afterward.
-/

mutual

inductive Expr where
  | name          : String → SourceSpan → Expr
  | constant      : Constant → SourceSpan → Expr
  | binOp         : Expr → BinOp → Expr → SourceSpan → Expr
  | unaryOp       : UnaryOp → Expr → SourceSpan → Expr
  | boolOp        : BoolOp → List Expr → SourceSpan → Expr
  | compare       : Expr → List (CmpOp × Expr) → SourceSpan → Expr
  | call          : Expr → List Expr → List CallKeyword → SourceSpan → Expr
  | attribute     : Expr → String → SourceSpan → Expr
  | subscript     : Expr → Expr → SourceSpan → Expr
  | starred       : Expr → SourceSpan → Expr
  | ifExp         : Expr → Expr → Expr → SourceSpan → Expr
  | lambda_       : Arguments → Expr → SourceSpan → Expr
  | listComp      : Expr → List Comprehension → SourceSpan → Expr
  | setComp       : Expr → List Comprehension → SourceSpan → Expr
  | dictComp      : Expr → Expr → List Comprehension → SourceSpan → Expr
  | generatorExp  : Expr → List Comprehension → SourceSpan → Expr
  | await_        : Expr → SourceSpan → Expr
  | yield_        : Option Expr → SourceSpan → Expr
  | yieldFrom     : Expr → SourceSpan → Expr
  | fstring       : List Expr → SourceSpan → Expr
  | formattedValue : Expr → Option Char → Option Expr → SourceSpan → Expr
  | namedExpr     : Expr → Expr → SourceSpan → Expr
  | slice         : Option Expr → Option Expr → Option Expr → SourceSpan → Expr
  | tuple         : List Expr → SourceSpan → Expr
  | list_         : List Expr → SourceSpan → Expr
  | set_          : List Expr → SourceSpan → Expr
  | dict          : List (Option Expr × Expr) → SourceSpan → Expr

/-- A single `for ... in ... [if ...]` clause in a comprehension. -/
inductive Comprehension where
  | mk : (target : Expr) → (iter : Expr) → (ifs : List Expr) →
         (isAsync : Bool) → Comprehension

/-- A single parameter in a function signature. -/
inductive Arg where
  | mk : (name : String) → (annotation : Option Expr) →
         (span : SourceSpan) → Arg

/-- Full parameter list for a function or lambda. -/
inductive Arguments where
  | mk : (posOnlyArgs : List Arg) → (args : List Arg) →
         (varArg : Option Arg) → (kwOnlyArgs : List Arg) →
         (kwDefaults : List (Option Expr)) → (kwArg : Option Arg) →
         (defaults : List Expr) → Arguments

/-- A keyword argument in a function call (e.g. `f(x=1)` or `f(**d)`). -/
inductive CallKeyword where
  | mk : (name : Option String) → (value : Expr) →
         (span : SourceSpan) → CallKeyword

end

-- Inhabited instances for mutual types (needed for [i]! indexing in interpreter)
instance : Inhabited Expr where
  default := .constant .none_ { start := ⟨0, 0, 0⟩, stop := ⟨0, 0, 0⟩ }

instance : Inhabited Arg where
  default := .mk "" none { start := ⟨0, 0, 0⟩, stop := ⟨0, 0, 0⟩ }

instance : Inhabited CallKeyword where
  default := .mk none (default : Expr) { start := ⟨0, 0, 0⟩, stop := ⟨0, 0, 0⟩ }

instance : Inhabited Comprehension where
  default := .mk default default [] false

/-! ## Match patterns (reference `Expr` but not `Stmt`) -/

-- MatchPattern is self-recursive and references Expr but does not need
-- to be in the Expr mutual block since Expr does not reference it back.
inductive MatchPattern where
  | matchValue    : Expr → MatchPattern
  | matchClass    : Expr → List String → List MatchPattern → MatchPattern
  | matchWildcard : MatchPattern
  | matchCapture  : String → Option MatchPattern → MatchPattern
  | matchOr       : List MatchPattern → MatchPattern

instance : Inhabited MatchPattern where
  default := .matchWildcard

/-! ## Standalone types (reference `Expr` only, not `Stmt`) -/

/-- Import alias: `name [as asName]`. -/
structure Alias where
  name   : String
  asName : Option String
  span   : SourceSpan
  deriving Repr, BEq, Inhabited

/-- A single item in a `with` statement. -/
structure WithItem where
  contextExpr  : Expr
  optionalVars : Option Expr

/-! ## Statement-level mutual block

`Stmt`, `ExceptHandler`, `FunctionDef`, and `ClassDef` are mutually recursive:
`Stmt` references the others, and they contain `List Stmt` for bodies.
-/

mutual

inductive Stmt where
  | expr           : Expr → SourceSpan → Stmt
  | assign         : List Expr → Expr → SourceSpan → Stmt
  | augAssign      : Expr → BinOp → Expr → SourceSpan → Stmt
  | annAssign      : Expr → Expr → Option Expr → Bool → SourceSpan → Stmt
  | return_        : Option Expr → SourceSpan → Stmt
  | delete         : List Expr → SourceSpan → Stmt
  | raise_         : Option Expr → Option Expr → SourceSpan → Stmt
  | assert_        : Expr → Option Expr → SourceSpan → Stmt
  | import_        : List Alias → SourceSpan → Stmt
  | importFrom     : Option String → List Alias → Option Nat → SourceSpan → Stmt
  | if_            : Expr → List Stmt → List Stmt → SourceSpan → Stmt
  | while_         : Expr → List Stmt → List Stmt → SourceSpan → Stmt
  | for_           : Expr → Expr → List Stmt → List Stmt → SourceSpan → Stmt
  | with_          : List WithItem → List Stmt → SourceSpan → Stmt
  | try_           : List Stmt → List ExceptHandler → List Stmt → List Stmt →
                     SourceSpan → Stmt
  | functionDef    : FunctionDef → Stmt
  | asyncFunctionDef : FunctionDef → Stmt
  | classDef       : ClassDef → Stmt
  | global_        : List String → SourceSpan → Stmt
  | nonlocal_      : List String → SourceSpan → Stmt
  | pass_          : SourceSpan → Stmt
  | break_         : SourceSpan → Stmt
  | continue_      : SourceSpan → Stmt
  | asyncFor       : Expr → Expr → List Stmt → List Stmt → SourceSpan → Stmt
  | asyncWith      : List WithItem → List Stmt → SourceSpan → Stmt
  | match_         : Expr → List MatchCase → SourceSpan → Stmt
  | typeAlias      : String → Expr → SourceSpan → Stmt

/-- A single `case` clause in a `match` statement. -/
inductive MatchCase where
  | mk : (pattern : MatchPattern) → (guard : Option Expr) →
         (body : List Stmt) → (span : SourceSpan) → MatchCase

/-- An `except` clause in a `try` statement. -/
inductive ExceptHandler where
  | mk : (type_ : Option Expr) → (name : Option String) →
         (body : List Stmt) → (span : SourceSpan) → ExceptHandler

/-- A function definition (used for both `def` and `async def`). -/
inductive FunctionDef where
  | mk : (name : String) → (args : Arguments) → (body : List Stmt) →
         (decoratorList : List Expr) → (returns : Option Expr) →
         (span : SourceSpan) → FunctionDef

/-- A class definition. -/
inductive ClassDef where
  | mk : (name : String) → (bases : List Expr) →
         (keywords : List CallKeyword) → (body : List Stmt) →
         (decoratorList : List Expr) → (span : SourceSpan) → ClassDef

end

/-! ## Top-level module -/

/-- A parsed Python module. -/
inductive Module where
  | module : List Stmt → Module

/-! ## Accessor functions for single-constructor inductives -/

namespace Comprehension
def target : Comprehension → Expr | .mk t _ _ _ => t
def iter : Comprehension → Expr | .mk _ i _ _ => i
def ifs : Comprehension → List Expr | .mk _ _ is_ _ => is_
def isAsync : Comprehension → Bool | .mk _ _ _ a => a
end Comprehension

namespace Arg
def name : Arg → String | .mk n _ _ => n
def annotation : Arg → Option Expr | .mk _ a _ => a
def span : Arg → SourceSpan | .mk _ _ s => s
end Arg

namespace Arguments
def posOnlyArgs : Arguments → List Arg | .mk p _ _ _ _ _ _ => p
def args : Arguments → List Arg | .mk _ a _ _ _ _ _ => a
def varArg : Arguments → Option Arg | .mk _ _ v _ _ _ _ => v
def kwOnlyArgs : Arguments → List Arg | .mk _ _ _ k _ _ _ => k
def kwDefaults : Arguments → List (Option Expr) | .mk _ _ _ _ d _ _ => d
def kwArg : Arguments → Option Arg | .mk _ _ _ _ _ k _ => k
def defaults : Arguments → List Expr | .mk _ _ _ _ _ _ d => d
end Arguments

namespace CallKeyword
def name : CallKeyword → Option String | .mk n _ _ => n
def value : CallKeyword → Expr | .mk _ v _ => v
def span : CallKeyword → SourceSpan | .mk _ _ s => s
end CallKeyword

namespace MatchCase
def pattern : MatchCase → MatchPattern | .mk p _ _ _ => p
def guard : MatchCase → Option Expr | .mk _ g _ _ => g
def body : MatchCase → List Stmt | .mk _ _ b _ => b
def span : MatchCase → SourceSpan | .mk _ _ _ s => s
end MatchCase

namespace ExceptHandler
def type_ : ExceptHandler → Option Expr | .mk t _ _ _ => t
def name : ExceptHandler → Option String | .mk _ n _ _ => n
def body : ExceptHandler → List Stmt | .mk _ _ b _ => b
def span : ExceptHandler → SourceSpan | .mk _ _ _ s => s
end ExceptHandler

namespace FunctionDef
def name : FunctionDef → String | .mk n _ _ _ _ _ => n
def args : FunctionDef → Arguments | .mk _ a _ _ _ _ => a
def body : FunctionDef → List Stmt | .mk _ _ b _ _ _ => b
def decoratorList : FunctionDef → List Expr | .mk _ _ _ d _ _ => d
def returns : FunctionDef → Option Expr | .mk _ _ _ _ r _ => r
def span : FunctionDef → SourceSpan | .mk _ _ _ _ _ s => s
end FunctionDef

namespace ClassDef
def name : ClassDef → String | .mk n _ _ _ _ _ => n
def bases : ClassDef → List Expr | .mk _ b _ _ _ _ => b
def keywords : ClassDef → List CallKeyword | .mk _ _ k _ _ _ => k
def body : ClassDef → List Stmt | .mk _ _ _ b _ _ => b
def decoratorList : ClassDef → List Expr | .mk _ _ _ _ d _ => d
def span : ClassDef → SourceSpan | .mk _ _ _ _ _ s => s
end ClassDef

/-! ## Empty arguments helper -/

def Arguments.empty : Arguments :=
  .mk [] [] none [] [] none []

/-! ## Dump functions (for test assertions) -/

private def dumpList {α : Type} (f : α → String) (xs : List α) : String :=
  "[" ++ ", ".intercalate (xs.map f) ++ "]"

private def dumpOpt {α : Type} (f : α → String) : Option α → String
  | some a => f a
  | none   => "None"

def dumpConst : Constant → String
  | .none_      => "None"
  | .true_      => "True"
  | .false_     => "False"
  | .ellipsis   => "Ellipsis"
  | .int n      => s!"Int({n})"
  | .float_ f   => s!"Float({f})"
  | .imaginary f => s!"Imag({f})"
  | .string s   => s!"Str({s})"
  | .bytes _    => "Bytes(...)"

def dumpBinOp : BinOp → String
  | .add => "Add" | .sub => "Sub" | .mult => "Mult"
  | .matMult => "MatMult" | .div => "Div" | .mod => "Mod"
  | .pow => "Pow" | .lShift => "LShift" | .rShift => "RShift"
  | .bitOr => "BitOr" | .bitXor => "BitXor" | .bitAnd => "BitAnd"
  | .floorDiv => "FloorDiv"

def dumpUnaryOp : UnaryOp → String
  | .invert => "Invert" | .not_ => "Not" | .uAdd => "UAdd" | .uSub => "USub"

def dumpBoolOp : BoolOp → String
  | .and_ => "And" | .or_ => "Or"

def dumpCmpOp : CmpOp → String
  | .eq => "Eq" | .notEq => "NotEq" | .lt => "Lt" | .ltE => "LtE"
  | .gt => "Gt" | .gtE => "GtE" | .is_ => "Is" | .isNot => "IsNot"
  | .in_ => "In" | .notIn => "NotIn"

def dumpAlias : Alias → String
  | ⟨n, a, _⟩ => s!"Alias({n}, {dumpOpt id a})"

mutual

partial def dumpExpr : Expr → String
  | .name n _           => s!"Name({n})"
  | .constant c _       => dumpConst c
  | .binOp l op r _     => s!"BinOp({dumpExpr l}, {dumpBinOp op}, {dumpExpr r})"
  | .unaryOp op e _     => s!"UnaryOp({dumpUnaryOp op}, {dumpExpr e})"
  | .boolOp op es _     => s!"BoolOp({dumpBoolOp op}, {dumpList dumpExpr es})"
  | .compare l cs _     => s!"Compare({dumpExpr l}, {dumpList dumpCmpPair cs})"
  | .call f args kw _   => s!"Call({dumpExpr f}, {dumpList dumpExpr args}, {dumpList dumpCallKw kw})"
  | .attribute e a _    => s!"Attr({dumpExpr e}, {a})"
  | .subscript e s _    => s!"Subscript({dumpExpr e}, {dumpExpr s})"
  | .starred e _        => s!"Starred({dumpExpr e})"
  | .ifExp test t f _   => s!"IfExp({dumpExpr test}, {dumpExpr t}, {dumpExpr f})"
  | .lambda_ _ b _      => s!"Lambda({dumpExpr b})"
  | .listComp e cs _    => s!"ListComp({dumpExpr e}, {dumpList dumpComp cs})"
  | .setComp e cs _     => s!"SetComp({dumpExpr e}, {dumpList dumpComp cs})"
  | .dictComp k v cs _  => s!"DictComp({dumpExpr k}, {dumpExpr v}, {dumpList dumpComp cs})"
  | .generatorExp e cs _ => s!"GenExpr({dumpExpr e}, {dumpList dumpComp cs})"
  | .await_ e _         => s!"Await({dumpExpr e})"
  | .yield_ e _         => s!"Yield({dumpOpt dumpExpr e})"
  | .yieldFrom e _      => s!"YieldFrom({dumpExpr e})"
  | .fstring es _       => s!"FString({dumpList dumpExpr es})"
  | .formattedValue e _ _ _ => s!"FormattedValue({dumpExpr e})"
  | .namedExpr t v _    => s!"NamedExpr({dumpExpr t}, {dumpExpr v})"
  | .slice l u s _      => s!"Slice({dumpOpt dumpExpr l}, {dumpOpt dumpExpr u}, {dumpOpt dumpExpr s})"
  | .tuple es _         => s!"Tuple({dumpList dumpExpr es})"
  | .list_ es _         => s!"List({dumpList dumpExpr es})"
  | .set_ es _          => s!"Set({dumpList dumpExpr es})"
  | .dict ps _          => s!"Dict({dumpList dumpDictPair ps})"

partial def dumpStmt : Stmt → String
  | .expr e _              => s!"Expr({dumpExpr e})"
  | .assign ts v _         => s!"Assign({dumpList dumpExpr ts}, {dumpExpr v})"
  | .augAssign t op v _    => s!"AugAssign({dumpExpr t}, {dumpBinOp op}, {dumpExpr v})"
  | .annAssign t a v _ _   => s!"AnnAssign({dumpExpr t}, {dumpExpr a}, {dumpOpt dumpExpr v})"
  | .return_ e _           => s!"Return({dumpOpt dumpExpr e})"
  | .delete ts _           => s!"Delete({dumpList dumpExpr ts})"
  | .raise_ e c _          => s!"Raise({dumpOpt dumpExpr e}, {dumpOpt dumpExpr c})"
  | .assert_ t m _         => s!"Assert({dumpExpr t}, {dumpOpt dumpExpr m})"
  | .import_ as_ _         => s!"Import({dumpList dumpAlias as_})"
  | .importFrom m as_ _ _  => s!"ImportFrom({dumpOpt id m}, {dumpList dumpAlias as_})"
  | .if_ t b e _           => s!"If({dumpExpr t}, {dumpList dumpStmt b}, {dumpList dumpStmt e})"
  | .while_ t b e _        => s!"While({dumpExpr t}, {dumpList dumpStmt b}, {dumpList dumpStmt e})"
  | .for_ t i b e _        => s!"For({dumpExpr t}, {dumpExpr i}, {dumpList dumpStmt b}, {dumpList dumpStmt e})"
  | .with_ is_ b _         => s!"With({dumpList dumpWithItem is_}, {dumpList dumpStmt b})"
  | .try_ b hs e f _       => s!"Try({dumpList dumpStmt b}, {dumpList dumpHandler hs}, {dumpList dumpStmt e}, {dumpList dumpStmt f})"
  | .functionDef fd        => dumpFuncDef "FunctionDef" fd
  | .asyncFunctionDef fd   => dumpFuncDef "AsyncFunctionDef" fd
  | .classDef cd           => dumpClassDef_ cd
  | .global_ ns _          => s!"Global({dumpList id ns})"
  | .nonlocal_ ns _        => s!"Nonlocal({dumpList id ns})"
  | .pass_ _               => "Pass"
  | .break_ _              => "Break"
  | .continue_ _           => "Continue"
  | .asyncFor t i b e _    => s!"AsyncFor({dumpExpr t}, {dumpExpr i}, {dumpList dumpStmt b}, {dumpList dumpStmt e})"
  | .asyncWith is_ b _     => s!"AsyncWith({dumpList dumpWithItem is_}, {dumpList dumpStmt b})"
  | .match_ s cs _         => s!"Match({dumpExpr s}, {dumpList dumpMatchCase cs})"
  | .typeAlias n e _       => s!"TypeAlias({n}, {dumpExpr e})"

partial def dumpMatchPattern : MatchPattern → String
  | .matchValue e          => s!"MatchValue({dumpExpr e})"
  | .matchClass c ns ps    => s!"MatchClass({dumpExpr c}, {dumpList id ns}, {dumpList dumpMatchPattern ps})"
  | .matchWildcard         => "MatchWildcard"
  | .matchCapture n p      => s!"MatchCapture({n}, {dumpOpt dumpMatchPattern p})"
  | .matchOr ps            => s!"MatchOr({dumpList dumpMatchPattern ps})"

partial def dumpMatchCase : MatchCase → String
  | .mk p g b _ => s!"Case({dumpMatchPattern p}, {dumpOpt dumpExpr g}, {dumpList dumpStmt b})"

partial def dumpComp : Comprehension → String
  | .mk t i ifs_ a => s!"Comp({dumpExpr t}, {dumpExpr i}, {dumpList dumpExpr ifs_}, {a})"

partial def dumpCallKw : CallKeyword → String
  | .mk n v _ => s!"Kw({dumpOpt id n}, {dumpExpr v})"

partial def dumpCmpPair : CmpOp × Expr → String
  | (op, e) => s!"({dumpCmpOp op}, {dumpExpr e})"

partial def dumpDictPair : Option Expr × Expr → String
  | (k, v) => s!"({dumpOpt dumpExpr k}, {dumpExpr v})"

partial def dumpHandler : ExceptHandler → String
  | .mk t n b _ => s!"Handler({dumpOpt dumpExpr t}, {dumpOpt id n}, {dumpList dumpStmt b})"

partial def dumpFuncDef : String → FunctionDef → String
  | tag, .mk n _ b _ _ _ => s!"{tag}({n}, {dumpList dumpStmt b})"

partial def dumpClassDef_ : ClassDef → String
  | .mk n bases _ b _ _ =>
    s!"ClassDef({n}, {dumpList dumpExpr bases}, {dumpList dumpStmt b})"

partial def dumpWithItem : WithItem → String
  | ⟨e, v⟩ => s!"WithItem({dumpExpr e}, {dumpOpt dumpExpr v})"

end

/-- Dump a module to a string for testing. -/
def dumpModule : Module → String
  | .module stmts => s!"Module({dumpList dumpStmt stmts})"

-- ============================================================
-- Yield detection (for identifying generator functions)
-- ============================================================

mutual

partial def exprContainsYield : Expr → Bool
  | .yield_ _ _ => true
  | .yieldFrom _ _ => true
  | .binOp l _ r _ => exprContainsYield l || exprContainsYield r
  | .unaryOp _ e _ => exprContainsYield e
  | .boolOp _ es _ => es.any exprContainsYield
  | .compare e cs _ => exprContainsYield e || cs.any fun (_, ce) => exprContainsYield ce
  | .call f args kw _ =>
    exprContainsYield f || args.any exprContainsYield
    || kw.any fun k => exprContainsYield k.value
  | .attribute e _ _ => exprContainsYield e
  | .subscript e i _ => exprContainsYield e || exprContainsYield i
  | .starred e _ => exprContainsYield e
  | .ifExp t b o _ => exprContainsYield t || exprContainsYield b || exprContainsYield o
  | .lambda_ _ _ _ => false  -- nested lambda has its own generator status
  | .listComp e cs _ => exprContainsYield e || cs.any compContainsYield
  | .setComp e cs _ => exprContainsYield e || cs.any compContainsYield
  | .dictComp k v cs _ =>
    exprContainsYield k || exprContainsYield v || cs.any compContainsYield
  | .generatorExp e cs _ => exprContainsYield e || cs.any compContainsYield
  | .await_ e _ => exprContainsYield e
  | .fstring es _ => es.any exprContainsYield
  | .formattedValue e _ _ _ => exprContainsYield e
  | .namedExpr t v _ => exprContainsYield t || exprContainsYield v
  | .slice l u s _ =>
    (match l with | some e => exprContainsYield e | none => false)
    || (match u with | some e => exprContainsYield e | none => false)
    || (match s with | some e => exprContainsYield e | none => false)
  | .tuple es _ => es.any exprContainsYield
  | .list_ es _ => es.any exprContainsYield
  | .set_ es _ => es.any exprContainsYield
  | .dict ps _ => ps.any fun (k, v) =>
    (match k with | some e => exprContainsYield e | none => false) || exprContainsYield v
  | _ => false

partial def stmtContainsYield : Stmt → Bool
  | .expr e _ => exprContainsYield e
  | .assign ts v _ => ts.any exprContainsYield || exprContainsYield v
  | .augAssign t _ v _ => exprContainsYield t || exprContainsYield v
  | .annAssign _ _ v _ _ =>
    match v with | some e => exprContainsYield e | none => false
  | .return_ e _ => match e with | some ex => exprContainsYield ex | none => false
  | .delete ts _ => ts.any exprContainsYield
  | .raise_ e c _ =>
    (match e with | some ex => exprContainsYield ex | none => false)
    || (match c with | some cx => exprContainsYield cx | none => false)
  | .assert_ t m _ =>
    exprContainsYield t
    || (match m with | some mx => exprContainsYield mx | none => false)
  | .if_ t b e _ => exprContainsYield t || stmtsContainYield b || stmtsContainYield e
  | .while_ t b e _ => exprContainsYield t || stmtsContainYield b || stmtsContainYield e
  | .for_ _ i b e _ => exprContainsYield i || stmtsContainYield b || stmtsContainYield e
  | .with_ _ b _ => stmtsContainYield b
  | .try_ b hs e f _ =>
    stmtsContainYield b || hs.any (fun h => stmtsContainYield h.body)
    || stmtsContainYield e || stmtsContainYield f
  | .global_ _ _ | .nonlocal_ _ _ | .pass_ _ | .break_ _ | .continue_ _ => false
  | .functionDef _ | .asyncFunctionDef _ | .classDef _ => false  -- nested defs are separate
  | .import_ _ _ | .importFrom _ _ _ _ => false
  | .asyncFor _ i b e _ => exprContainsYield i || stmtsContainYield b || stmtsContainYield e
  | .asyncWith _ b _ => stmtsContainYield b
  | .match_ e cs _ =>
    exprContainsYield e || cs.any fun c => stmtsContainYield c.body
  | .typeAlias _ e _ => exprContainsYield e

partial def compContainsYield : Comprehension → Bool
  | .mk t i ifs_ _ =>
    exprContainsYield t || exprContainsYield i || ifs_.any exprContainsYield

partial def stmtsContainYield : List Stmt → Bool
  | [] => false
  | s :: rest => stmtContainsYield s || stmtsContainYield rest

end

end LeanPython.AST
