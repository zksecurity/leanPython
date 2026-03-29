import LeanPython.Parser.Tokens
import LeanPython.AST.Types

set_option autoImplicit false

namespace LeanPython.Parser

open LeanPython.Lexer (SourceSpan Token TokenKind Keyword Operator Delimiter)
open LeanPython.AST (Expr Constant BinOp UnaryOp BoolOp CmpOp
                  Comprehension Arg Arguments CallKeyword)

/-- Try to consume a comparison operator, returning its `CmpOp`. -/
private def tryCmpOp : ParserM (Option CmpOp) := do
  match ← peekKind with
  | some (.operator .eqEqual)     => discard advance; return some .eq
  | some (.operator .notEqual)    => discard advance; return some .notEq
  | some (.operator .less)        => discard advance; return some .lt
  | some (.operator .lessEqual)   => discard advance; return some .ltE
  | some (.operator .greater)     => discard advance; return some .gt
  | some (.operator .greaterEqual) => discard advance; return some .gtE
  | some (.keyword .is_) =>
    discard advance
    if ← isKeyword .not_ then discard advance; return some .isNot
    return some .is_
  | some (.keyword .in_) => discard advance; return some .in_
  | some (.keyword .not_) =>
    let r ← attempt do discard advance; discard (expectKeyword .in_); return CmpOp.notIn
    return r
  | _ => return none

mutual

/-- Parse a single atom: literal, name, parenthesized expr, list, dict/set. -/
partial def parseAtom : ParserM Expr := do
  let tok ← match ← peek with
    | some t => pure t
    | none   => throw (.unexpectedEOF "expected expression")
  match tok.kind with
  | .integer n   => discard advance; return .constant (.int n) tok.span
  | .float_ f    => discard advance; return .constant (.float_ f) tok.span
  | .imaginary f => discard advance; return .constant (.imaginary f) tok.span
  | .string s    => discard advance
                     let mut result := s
                     let mut endSpan := tok.span
                     while true do
                       match ← peekKind with
                       | some (.string s2) =>
                         let tok2 ← advance
                         result := result ++ s2
                         endSpan := tok2.span
                       | _ => break
                     return .constant (.string result) { start := tok.span.start, stop := endSpan.stop }
  | .bytes b     => discard advance; return .constant (.bytes b) tok.span
  | .name n      => discard advance; return .name n tok.span
  | .keyword .true_  => discard advance; return .constant .true_ tok.span
  | .keyword .false_ => discard advance; return .constant .false_ tok.span
  | .keyword .none_  => discard advance; return .constant .none_ tok.span
  | .delimiter .ellipsis => discard advance; return .constant .ellipsis tok.span
  | .delimiter .lpar => parseParenExpr
  | .delimiter .lsqb => parseListOrListComp
  | .delimiter .lbrace => parseDictOrSetOrComp
  | _ => throw (.unexpectedToken tok.kind tok.span.start "expected expression")

/-- Parse `(expr)`, `(expr,)`, `(expr, expr, ...)`, or `(expr for ...)`. -/
partial def parseParenExpr : ParserM Expr := do
  let start ← currentSpan
  discard (expectDelimiter .lpar)
  if ← isDelimiter .rpar then
    discard advance
    return .tuple [] (← spanFrom start)
  let first ← parseTestListStarExprItem
  if ← isKeyword .for_ then
    let comps ← parseComprehensionClauses
    discard (expectDelimiter .rpar)
    return .generatorExp first comps (← spanFrom start)
  if ← isDelimiter .comma then
    let mut items := [first]
    while (← isDelimiter .comma) do
      discard advance
      if ← isDelimiter .rpar then break
      items := items ++ [← parseTestListStarExprItem]
    discard (expectDelimiter .rpar)
    return .tuple items (← spanFrom start)
  discard (expectDelimiter .rpar)
  return first

/-- Parse `[expr, ...]` or `[expr for ...]`. -/
partial def parseListOrListComp : ParserM Expr := do
  let start ← currentSpan
  discard (expectDelimiter .lsqb)
  if ← isDelimiter .rsqb then
    discard advance
    return .list_ [] (← spanFrom start)
  let first ← parseTestListStarExprItem
  if ← isKeyword .for_ then
    let comps ← parseComprehensionClauses
    discard (expectDelimiter .rsqb)
    return .listComp first comps (← spanFrom start)
  let mut items := [first]
  while (← isDelimiter .comma) do
    discard advance
    if ← isDelimiter .rsqb then break
    items := items ++ [← parseTestListStarExprItem]
  discard (expectDelimiter .rsqb)
  return .list_ items (← spanFrom start)

/-- Parse `{...}` — dict, set, dict comprehension, or set comprehension. -/
partial def parseDictOrSetOrComp : ParserM Expr := do
  let start ← currentSpan
  discard (expectDelimiter .lbrace)
  if ← isDelimiter .rbrace then
    discard advance
    return .dict [] (← spanFrom start)
  if ← isOperator .doubleStar then
    discard advance
    let val ← parseExpression
    let first : Option Expr × Expr := (none, val)
    let mut pairs := [first]
    while (← isDelimiter .comma) do
      discard advance
      if ← isDelimiter .rbrace then break
      pairs := pairs ++ [← parseDictEntry]
    discard (expectDelimiter .rbrace)
    return .dict pairs (← spanFrom start)
  let first ← parseTestListStarExprItem
  if ← isDelimiter .colon then
    discard advance
    let val ← parseTestListStarExprItem
    if ← isKeyword .for_ then
      let comps ← parseComprehensionClauses
      discard (expectDelimiter .rbrace)
      return .dictComp first val comps (← spanFrom start)
    let mut pairs : List (Option Expr × Expr) := [(some first, val)]
    while (← isDelimiter .comma) do
      discard advance
      if ← isDelimiter .rbrace then break
      pairs := pairs ++ [← parseDictEntry]
    discard (expectDelimiter .rbrace)
    return .dict pairs (← spanFrom start)
  if ← isKeyword .for_ then
    let comps ← parseComprehensionClauses
    discard (expectDelimiter .rbrace)
    return .setComp first comps (← spanFrom start)
  let mut items := [first]
  while (← isDelimiter .comma) do
    discard advance
    if ← isDelimiter .rbrace then break
    items := items ++ [← parseTestListStarExprItem]
  discard (expectDelimiter .rbrace)
  return .set_ items (← spanFrom start)

/-- Parse a dict entry: `expr : expr` or `**expr`. -/
partial def parseDictEntry : ParserM (Option Expr × Expr) := do
  if ← isOperator .doubleStar then
    discard advance
    let val ← parseExpression
    return (none, val)
  let key ← parseExpression
  discard (expectDelimiter .colon)
  let val ← parseExpression
  return (some key, val)

/-- Parse one or more `for target in iter [if cond]*` clauses. -/
partial def parseComprehensionClauses : ParserM (List Comprehension) := do
  let mut comps : List Comprehension := []
  while (← isKeyword .for_) do
    discard advance
    let target ← parseTargetList
    discard (expectKeyword .in_)
    let iter ← parseDisjunction
    let mut ifs : List Expr := []
    while (← isKeyword .if_) do
      discard advance
      ifs := ifs ++ [← parseDisjunction]
    comps := comps ++ [Comprehension.mk target iter ifs false]
  return comps

/-- Parse primary: atom followed by `.name`, `(args)`, `[subscript]`. -/
partial def parsePrimary : ParserM Expr := do
  let start ← currentSpan
  let mut expr ← parseAtom
  while true do
    if ← isDelimiter .dot then
      discard advance
      let (name, _) ← parseName
      expr := .attribute expr name (← spanFrom start)
    else if ← isDelimiter .lpar then
      expr ← parseCallArgs expr start
    else if ← isDelimiter .lsqb then
      discard advance
      let sub ← parseSubscriptExpr
      discard (expectDelimiter .rsqb)
      expr := .subscript expr sub (← spanFrom start)
    else
      break
  return expr

/-- Parse call arguments: `func(args)`. -/
partial def parseCallArgs (func : Expr) (start : SourceSpan) : ParserM Expr := do
  discard (expectDelimiter .lpar)
  if ← isDelimiter .rpar then
    discard advance
    return .call func [] [] (← spanFrom start)
  let (args, kwargs) ← parseArgList
  discard (expectDelimiter .rpar)
  return .call func args kwargs (← spanFrom start)

/-- Parse the argument list inside a call. -/
partial def parseArgList : ParserM (List Expr × List CallKeyword) := do
  let mut positional : List Expr := []
  let mut keywords : List CallKeyword := []
  let mut first := true
  while true do
    if !first then
      if ← isDelimiter .comma then discard advance else break
    first := false
    if ← isDelimiter .rpar then break
    if ← isOperator .doubleStar then
      let tok ← advance
      let val ← parseExpression
      keywords := keywords ++ [CallKeyword.mk none val (← spanFrom tok.span)]
    else if ← isOperator .star then
      let tok ← advance
      let val ← parseExpression
      positional := positional ++ [.starred val (← spanFrom tok.span)]
    else
      let kwResult ← attempt do
        let (n, sp) ← parseName
        discard (expectOperator .equal)
        let val ← parseExpression
        return (n, val, sp)
      match kwResult with
      | some (n, val, sp) =>
        keywords := keywords ++ [CallKeyword.mk (some n) val (← spanFrom sp)]
      | none =>
        positional := positional ++ [← parseExpression]
  return (positional, keywords)

/-- Parse subscript content: simple index, slice, or tuple of slices. -/
partial def parseSubscriptExpr : ParserM Expr := do
  let start ← currentSpan
  let first ← parseSliceOrExpr
  if ← isDelimiter .comma then
    let mut items := [first]
    while (← isDelimiter .comma) do
      discard advance
      if ← isDelimiter .rsqb then break
      items := items ++ [← parseSliceOrExpr]
    return .tuple items (← spanFrom start)
  return first

/-- Parse either a slice `[a:b:c]` or a regular expression. -/
partial def parseSliceOrExpr : ParserM Expr := do
  let start ← currentSpan
  if ← isDelimiter .colon then
    discard advance
    let upper ← if (← isDelimiter .colon) || (← isDelimiter .rsqb) || (← isDelimiter .comma)
                 then pure none
                 else some <$> parseExpression
    let step ← if ← isDelimiter .colon then do
                  discard advance
                  if (← isDelimiter .rsqb) || (← isDelimiter .comma)
                  then pure none
                  else some <$> parseExpression
                else pure none
    return .slice none upper step (← spanFrom start)
  let lower ← parseExpression
  if !(← isDelimiter .colon) then return lower
  discard advance
  let upper ← if (← isDelimiter .colon) || (← isDelimiter .rsqb) || (← isDelimiter .comma)
               then pure none
               else some <$> parseExpression
  let step ← if ← isDelimiter .colon then do
                discard advance
                if (← isDelimiter .rsqb) || (← isDelimiter .comma)
                then pure none
                else some <$> parseExpression
              else pure none
  return .slice (some lower) upper step (← spanFrom start)

/-- Parse `await primary` or just primary. -/
partial def parseAwaitExpr : ParserM Expr := do
  if ← isKeyword .await then
    let start ← currentSpan
    discard advance
    let e ← parsePrimary
    return .await_ e (← spanFrom start)
  parsePrimary

/-- Parse `await_expr ** factor` (right-associative) or just `await_expr`. -/
partial def parsePower : ParserM Expr := do
  let start ← currentSpan
  let base ← parseAwaitExpr
  if ← isOperator .doubleStar then
    discard advance
    let exp ← parseFactor
    return .binOp base .pow exp (← spanFrom start)
  return base

/-- Parse `+factor`, `-factor`, `~factor`, or power. -/
partial def parseFactor : ParserM Expr := do
  let start ← currentSpan
  if ← isOperator .plus then
    discard advance
    let e ← parseFactor
    return .unaryOp .uAdd e (← spanFrom start)
  if ← isOperator .minus then
    discard advance
    let e ← parseFactor
    return .unaryOp .uSub e (← spanFrom start)
  if ← isOperator .tilde then
    discard advance
    let e ← parseFactor
    return .unaryOp .invert e (← spanFrom start)
  parsePower

/-- Parse multiplicative expressions (*, /, //, %, @). -/
partial def parseTerm : ParserM Expr := do
  let start ← currentSpan
  let mut left ← parseFactor
  while true do
    if ← isOperator .star then
      discard advance; left := .binOp left .mult (← parseFactor) (← spanFrom start)
    else if ← isOperator .slash then
      discard advance; left := .binOp left .div (← parseFactor) (← spanFrom start)
    else if ← isOperator .doubleSlash then
      discard advance; left := .binOp left .floorDiv (← parseFactor) (← spanFrom start)
    else if ← isOperator .percent then
      discard advance; left := .binOp left .mod (← parseFactor) (← spanFrom start)
    else if ← isOperator .at then
      discard advance; left := .binOp left .matMult (← parseFactor) (← spanFrom start)
    else break
  return left

/-- Parse additive expressions (+, -). -/
partial def parseSum : ParserM Expr := do
  let start ← currentSpan
  let mut left ← parseTerm
  while true do
    if ← isOperator .plus then
      discard advance; left := .binOp left .add (← parseTerm) (← spanFrom start)
    else if ← isOperator .minus then
      discard advance; left := .binOp left .sub (← parseTerm) (← spanFrom start)
    else break
  return left

/-- Parse shift expressions (<<, >>). -/
partial def parseShift : ParserM Expr := do
  let start ← currentSpan
  let mut left ← parseSum
  while true do
    if ← isOperator .leftShift then
      discard advance; left := .binOp left .lShift (← parseSum) (← spanFrom start)
    else if ← isOperator .rightShift then
      discard advance; left := .binOp left .rShift (← parseSum) (← spanFrom start)
    else break
  return left

/-- Parse bitwise AND. -/
partial def parseBitwiseAnd : ParserM Expr := do
  let start ← currentSpan
  let mut left ← parseShift
  while (← isOperator .amper) do
    discard advance; left := .binOp left .bitAnd (← parseShift) (← spanFrom start)
  return left

/-- Parse bitwise XOR. -/
partial def parseBitwiseXor : ParserM Expr := do
  let start ← currentSpan
  let mut left ← parseBitwiseAnd
  while (← isOperator .circumflex) do
    discard advance; left := .binOp left .bitXor (← parseBitwiseAnd) (← spanFrom start)
  return left

/-- Parse bitwise OR. -/
partial def parseBitwiseOr : ParserM Expr := do
  let start ← currentSpan
  let mut left ← parseBitwiseXor
  while (← isOperator .vbar) do
    discard advance; left := .binOp left .bitOr (← parseBitwiseXor) (← spanFrom start)
  return left

/-- Parse chained comparisons (a < b <= c). -/
partial def parseComparison : ParserM Expr := do
  let start ← currentSpan
  let left ← parseBitwiseOr
  let mut comparators : List (CmpOp × Expr) := []
  while true do
    match ← tryCmpOp with
    | some op =>
      let right ← parseBitwiseOr
      comparators := comparators ++ [(op, right)]
    | none => break
  if comparators.isEmpty then return left
  return .compare left comparators (← spanFrom start)

/-- Parse `not expr`. -/
partial def parseInversion : ParserM Expr := do
  if ← isKeyword .not_ then
    let start ← currentSpan
    discard advance
    let e ← parseInversion
    return .unaryOp .not_ e (← spanFrom start)
  parseComparison

/-- Parse `expr and expr and ...`. -/
partial def parseConjunction : ParserM Expr := do
  let start ← currentSpan
  let first ← parseInversion
  let mut items := [first]
  while (← isKeyword .and_) do
    discard advance
    items := items ++ [← parseInversion]
  if items.length == 1 then return first
  return .boolOp .and_ items (← spanFrom start)

/-- Parse `expr or expr or ...`. -/
partial def parseDisjunction : ParserM Expr := do
  let start ← currentSpan
  let first ← parseConjunction
  let mut items := [first]
  while (← isKeyword .or_) do
    discard advance
    items := items ++ [← parseConjunction]
  if items.length == 1 then return first
  return .boolOp .or_ items (← spanFrom start)

/-- Parse `expr if condition else expr` (ternary). -/
partial def parseConditional : ParserM Expr := do
  let start ← currentSpan
  let body ← parseDisjunction
  if ← isKeyword .if_ then
    discard advance
    let test ← parseDisjunction
    discard (expectKeyword .else_)
    let orelse ← parseExpression
    return .ifExp test body orelse (← spanFrom start)
  return body

/-- Parse `lambda [params]: expr`. -/
partial def parseLambda : ParserM Expr := do
  if ← isKeyword .lambda_ then
    let start ← currentSpan
    discard advance
    let args ← parseLambdaParams
    discard (expectDelimiter .colon)
    let body ← parseExpression
    return .lambda_ args body (← spanFrom start)
  parseConditional

/-- Parse lambda parameters (simplified: comma-separated names with optional defaults). -/
partial def parseLambdaParams : ParserM Arguments := do
  if ← isDelimiter .colon then return Arguments.empty
  let params ← sepBy1 parseLambdaParam (discard (expectDelimiter .comma))
  return Arguments.mk [] params none [] [] none []

partial def parseLambdaParam : ParserM Arg := do
  let (n, sp) ← parseName
  if ← isOperator .equal then
    discard advance
    let _ ← parseExpression
    pure ()
  return Arg.mk n none sp

/-- Parse `name := expr` or fallthrough. -/
partial def parseNamedExpr : ParserM Expr := do
  let start ← currentSpan
  let expr ← parseLambda
  if ← isOperator .colonEqual then
    discard advance
    let value ← parseExpression
    return .namedExpr expr value (← spanFrom start)
  return expr

/-- Parse yield expression or fallthrough. -/
partial def parseYieldExpr : ParserM Expr := do
  if ← isKeyword .yield_ then
    let start ← currentSpan
    discard advance
    if ← isKeyword .from_ then
      discard advance
      let e ← parseExpression
      return .yieldFrom e (← spanFrom start)
    if (← isNewline) || (← isDelimiter .rpar) then
      return .yield_ none (← spanFrom start)
    let e ← parseExpressionList
    return .yield_ (some e) (← spanFrom start)
  parseNamedExpr

/-- Parse `*expr` (starred) or regular expression. -/
partial def parseStarExpr : ParserM Expr := do
  if ← isOperator .star then
    let start ← currentSpan
    discard advance
    let e ← parseBitwiseOr
    return .starred e (← spanFrom start)
  parseNamedExpr

/-- Parse a Python expression. Main entry point. -/
partial def parseExpression : ParserM Expr := parseYieldExpr

/-- Parse a starred or regular expression (for lists, tuples). -/
partial def parseTestListStarExprItem : ParserM Expr := parseStarExpr

/-- Parse a single target (primary-level only: name, attr, subscript, or starred). -/
partial def parseStarTarget : ParserM Expr := do
  if ← isOperator .star then
    let start ← currentSpan
    discard advance
    let e ← parsePrimary
    return .starred e (← spanFrom start)
  parsePrimary

/-- Parse a target expression for assignments/for-loops/comprehensions.
    Uses primary-level parsing to avoid consuming `in` as a comparison. -/
partial def parseTargetList : ParserM Expr := do
  let start ← currentSpan
  let first ← parseStarTarget
  if ← isDelimiter .comma then
    let mut items := [first]
    while (← isDelimiter .comma) do
      discard advance
      match ← peekKind with
      | some (.keyword .in_) | some .newline | some (.delimiter .colon) |
        some (.operator .equal) | some (.delimiter .rpar) |
        some (.delimiter .rsqb) => break
      | _ => items := items ++ [← parseStarTarget]
    return .tuple items (← spanFrom start)
  return first

/-- Parse a comma-separated expression list (may produce tuple). -/
partial def parseExpressionList : ParserM Expr := do
  let start ← currentSpan
  let first ← parseExpression
  if ← isDelimiter .comma then
    let mut items := [first]
    while (← isDelimiter .comma) do
      discard advance
      match ← peekKind with
      | some .newline | some (.delimiter .rpar) | some (.delimiter .rsqb) |
        some (.delimiter .rbrace) | some (.delimiter .colon) | some .endmarker => break
      | _ => items := items ++ [← parseExpression]
    if items.length == 1 then return first
    return .tuple items (← spanFrom start)
  return first

end -- mutual

end LeanPython.Parser
