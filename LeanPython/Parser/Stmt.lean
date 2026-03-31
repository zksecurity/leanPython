import LeanPython.Parser.Expr
import LeanPython.AST.Types

set_option autoImplicit false

namespace LeanPython.Parser

open LeanPython.Lexer (SourceSpan Token TokenKind Keyword Operator Delimiter)
open LeanPython.AST (Expr Stmt Alias WithItem ExceptHandler FunctionDef ClassDef
                  Module BinOp Arguments Arg CallKeyword MatchPattern MatchCase)

mutual

/-- Attach decorators to a function or class def. -/
partial def attachDecorators (stmt : Stmt) (decorators : List Expr) : Stmt :=
  match stmt with
  | .functionDef (.mk n a b _ r s) => .functionDef (.mk n a b decorators r s)
  | .asyncFunctionDef (.mk n a b _ r s) => .asyncFunctionDef (.mk n a b decorators r s)
  | .classDef (.mk n bs kw b _ s) => .classDef (.mk n bs kw b decorators s)
  | other => other

/-- Parse a single function parameter: `name [: annotation] [= default]`. -/
partial def parseParam : ParserM (Arg × Option Expr) := do
  let (n, sp) ← parseName
  let ann ← if ← isDelimiter .colon then do
               discard advance
               some <$> parseExpression
             else pure none
  let default_ ← if ← isOperator .equal then do
                    discard advance
                    some <$> parseExpression
                  else pure none
  return (Arg.mk n ann sp, default_)

/-- Parse function parameters between parentheses. -/
partial def parseFuncParams : ParserM Arguments := do
  discard (expectDelimiter .lpar)
  if ← isDelimiter .rpar then
    discard advance
    return Arguments.empty
  let mut args : List Arg := []
  let mut defaults : List Expr := []
  let mut varArg : Option Arg := none
  let mut kwOnly : List Arg := []
  let mut kwDefaults : List (Option Expr) := []
  let mut kwArg : Option Arg := none
  let mut seenStar := false
  let mut first := true
  while true do
    if !first then
      if ← isDelimiter .comma then discard advance else break
    first := false
    if ← isDelimiter .rpar then break
    -- `*` or `*name`
    if ← isOperator .star then
      discard advance
      seenStar := true
      if ← isName then
        let (n, sp) ← parseName
        let ann ← if ← isDelimiter .colon then do
                     discard advance; some <$> parseExpression
                   else pure none
        varArg := some (Arg.mk n ann sp)
      continue
    -- `**name`
    if ← isOperator .doubleStar then
      discard advance
      let (n, sp) ← parseName
      let ann ← if ← isDelimiter .colon then do
                   discard advance; some <$> parseExpression
                 else pure none
      kwArg := some (Arg.mk n ann sp)
      continue
    -- Regular parameter
    let (param, default_) ← parseParam
    if seenStar then
      kwOnly := kwOnly ++ [param]
      kwDefaults := kwDefaults ++ [default_]
    else
      args := args ++ [param]
      match default_ with
      | some d => defaults := defaults ++ [d]
      | none => pure ()
  discard (expectDelimiter .rpar)
  return Arguments.mk [] args varArg kwOnly kwDefaults kwArg defaults

/-- Parse a dotted name like `a.b.c`. -/
partial def parseDottedName : ParserM String := do
  let (first, _) ← parseName
  let mut result := first
  while (← isDelimiter .dot) do
    discard advance
    let (next, _) ← parseName
    result := result ++ "." ++ next
  return result

/-- Parse `name [as alias]` for imports. -/
partial def parseImportAlias : ParserM Alias := do
  let start ← currentSpan
  let name ← parseDottedName
  let asName ← if ← isKeyword .as_ then do
                  discard advance
                  let (n, _) ← parseName
                  pure (some n)
                else pure none
  return { name := name, asName := asName, span := ← spanFrom start }

/-- Parse `name [as alias]` for `from ... import name`. -/
partial def parseFromImportAlias : ParserM Alias := do
  let start ← currentSpan
  let (name, _) ← parseName
  let asName ← if ← isKeyword .as_ then do
                  discard advance
                  let (n, _) ← parseName
                  pure (some n)
                else pure none
  return { name := name, asName := asName, span := ← spanFrom start }

-- ## Block parsing

/-- Parse an indented block: NEWLINE INDENT stmt+ DEDENT. -/
partial def parseBlock : ParserM (List Stmt) := do
  discard expectNewline
  discard expectIndent
  let stmts ← parseStatements
  discard expectDedent
  return stmts

/-- Parse one or more statements (until DEDENT or ENDMARKER). -/
partial def parseStatements : ParserM (List Stmt) := do
  let mut stmts : List Stmt := []
  while true do
    match ← peekKind with
    | some .dedent | some .endmarker | none => break
    | some .newline => discard advance
    | _ =>
      let newStmts ← parseStmtLine
      stmts := stmts ++ newStmts
  return stmts

/-- Parse a single line of statements (simple_stmts or compound_stmt). -/
partial def parseStmtLine : ParserM (List Stmt) := do
  match ← peekKind with
  | some (.keyword .if_)       => return [← parseIf]
  | some (.keyword .while_)    => return [← parseWhile]
  | some (.keyword .for_)      => return [← parseFor]
  | some (.keyword .try_)      => return [← parseTry]
  | some (.keyword .with_)     => return [← parseWith]
  | some (.keyword .def_)      => return [← parseFunctionDef false]
  | some (.keyword .class_)    => return [← parseClassDef false]
  | some (.keyword .async)     => return [← parseAsync]
  | some (.operator .at)       => return [← parseDecorated]
  | some (.name "match") =>
    match ← attempt parseMatchStmt with
    | some s => return [s]
    | none => parseSimpleStmts
  | _ => parseSimpleStmts

/-- Parse simple statements on one line, separated by `;`. -/
partial def parseSimpleStmts : ParserM (List Stmt) := do
  let first ← parseSimpleStmt
  let mut stmts := [first]
  while (← isDelimiter .semi) do
    discard advance
    if (← isNewline) || (← isEndmarker) then break
    stmts := stmts ++ [← parseSimpleStmt]
  if ← isNewline then discard advance
  return stmts

-- ## Simple statements

/-- Parse a single simple statement. -/
partial def parseSimpleStmt : ParserM Stmt := do
  let start ← currentSpan
  match ← peekKind with
  | some (.keyword .return_) => parseReturn
  | some (.keyword .pass_)   => discard advance; return .pass_ (← spanFrom start)
  | some (.keyword .break_)  => discard advance; return .break_ (← spanFrom start)
  | some (.keyword .continue_) => discard advance; return .continue_ (← spanFrom start)
  | some (.keyword .raise)   => parseRaise
  | some (.keyword .assert_) => parseAssert
  | some (.keyword .global)  => parseGlobal
  | some (.keyword .nonlocal) => parseNonlocal
  | some (.keyword .del)     => parseDel
  | some (.keyword .import_) => parseImport
  | some (.keyword .from_)   => parseFromImport
  | some (.keyword .yield_)  => do
    let e ← parseExpression
    return .expr e (← spanFrom start)
  | _ => parseExprOrAssign

/-- Parse `return [expr]`. -/
partial def parseReturn : ParserM Stmt := do
  let start ← currentSpan
  discard advance
  if (← isNewline) || (← isEndmarker) || (← isDelimiter .semi) then
    return .return_ none (← spanFrom start)
  let e ← parseExpressionList
  return .return_ (some e) (← spanFrom start)

/-- Parse `raise [expr [from expr]]`. -/
partial def parseRaise : ParserM Stmt := do
  let start ← currentSpan
  discard advance
  if (← isNewline) || (← isEndmarker) || (← isDelimiter .semi) then
    return .raise_ none none (← spanFrom start)
  let e ← parseExpression
  let cause ← if ← isKeyword .from_ then do
                 discard advance
                 some <$> parseExpression
               else pure none
  return .raise_ (some e) cause (← spanFrom start)

/-- Parse `assert expr [, msg]`. -/
partial def parseAssert : ParserM Stmt := do
  let start ← currentSpan
  discard advance
  let test ← parseExpression
  let msg ← if ← isDelimiter .comma then do
               discard advance
               some <$> parseExpression
             else pure none
  return .assert_ test msg (← spanFrom start)

/-- Parse `global name, name, ...`. -/
partial def parseGlobal : ParserM Stmt := do
  let start ← currentSpan
  discard advance
  let names ← sepBy1 (do let (n, _) ← parseName; return n) (discard (expectDelimiter .comma))
  return .global_ names (← spanFrom start)

/-- Parse `nonlocal name, name, ...`. -/
partial def parseNonlocal : ParserM Stmt := do
  let start ← currentSpan
  discard advance
  let names ← sepBy1 (do let (n, _) ← parseName; return n) (discard (expectDelimiter .comma))
  return .nonlocal_ names (← spanFrom start)

/-- Parse `del target, target, ...`. -/
partial def parseDel : ParserM Stmt := do
  let start ← currentSpan
  discard advance
  let targets ← sepBy1 parsePrimary (discard (expectDelimiter .comma))
  return .delete targets (← spanFrom start)

/-- Parse `import module [as alias], ...`. -/
partial def parseImport : ParserM Stmt := do
  let start ← currentSpan
  discard advance
  let aliases ← sepBy1 parseImportAlias (discard (expectDelimiter .comma))
  return .import_ aliases (← spanFrom start)

/-- Parse `from [.]* module import name [as alias], ...`. -/
partial def parseFromImport : ParserM Stmt := do
  let start ← currentSpan
  discard advance
  let mut level : Nat := 0
  while (← isDelimiter .dot) || (← isDelimiter .ellipsis) do
    match ← peekKind with
    | some (.delimiter .ellipsis) => discard advance; level := level + 3
    | some (.delimiter .dot) => discard advance; level := level + 1
    | _ => break
  let modName ← if ← isKeyword .import_ then pure none
                 else some <$> parseDottedName
  discard (expectKeyword .import_)
  if ← isOperator .star then
    discard advance
    let aliases := [{ name := "*", asName := none, span := ← spanFrom start : Alias }]
    let lvl := if level > 0 then some level else none
    return .importFrom modName aliases lvl (← spanFrom start)
  let hasParen ← if ← isDelimiter .lpar then do discard advance; pure true else pure false
  let aliases ← sepBy1 parseFromImportAlias (discard (expectDelimiter .comma))
  if hasParen then
    if ← isDelimiter .comma then discard advance
    discard (expectDelimiter .rpar)
  let lvl := if level > 0 then some level else none
  return .importFrom modName aliases lvl (← spanFrom start)

/-- Parse expression statement, or assignment/augmented assignment. -/
partial def parseExprOrAssign : ParserM Stmt := do
  let start ← currentSpan
  let lhs ← parseTestListStarExprItem
  match ← tryAugAssignOp with
  | some op =>
    let rhs ← parseExpressionList
    return .augAssign lhs op rhs (← spanFrom start)
  | none => pure ()
  if ← isDelimiter .colon then
    discard advance
    let ann ← parseExpression
    let val ← if ← isOperator .equal then do
                 discard advance
                 some <$> parseExpressionList
               else pure none
    return .annAssign lhs ann val true (← spanFrom start)
  if ← isOperator .equal then
    let mut targets := [lhs]
    while (← isOperator .equal) do
      discard advance
      let next ← parseExpressionList
      targets := targets ++ [next]
    match targets.reverse with
    | val :: revTargets =>
      return .assign revTargets.reverse val (← spanFrom start)
    | [] => throw (.invalidSyntax "empty assignment" (← spanFrom start))
  if ← isDelimiter .comma then
    let mut items := [lhs]
    while (← isDelimiter .comma) do
      discard advance
      if (← isNewline) || (← isEndmarker) || (← isDelimiter .semi) then break
      items := items ++ [← parseTestListStarExprItem]
    let tupleExpr := Expr.tuple items (← spanFrom start)
    if ← isOperator .equal then
      discard advance
      let val ← parseExpressionList
      return .assign [tupleExpr] val (← spanFrom start)
    return .expr tupleExpr (← spanFrom start)
  return .expr lhs (← spanFrom start)

-- ## Compound statements

/-- Parse `if expr: block [elif expr: block]* [else: block]`. -/
partial def parseIf : ParserM Stmt := do
  let start ← currentSpan
  discard (expectKeyword .if_)
  let test ← parseExpression
  discard (expectDelimiter .colon)
  let body ← parseBlock
  let mut orelse : List Stmt := []
  if ← isKeyword .elif then
    orelse := [← parseElif]
  else if ← isKeyword .else_ then
    discard advance
    discard (expectDelimiter .colon)
    orelse ← parseBlock
  return .if_ test body orelse (← spanFrom start)

/-- Parse `elif expr: block [elif ...]* [else: block]` as nested if. -/
partial def parseElif : ParserM Stmt := do
  let start ← currentSpan
  discard (expectKeyword .elif)
  let test ← parseExpression
  discard (expectDelimiter .colon)
  let body ← parseBlock
  let mut orelse : List Stmt := []
  if ← isKeyword .elif then
    orelse := [← parseElif]
  else if ← isKeyword .else_ then
    discard advance
    discard (expectDelimiter .colon)
    orelse ← parseBlock
  return .if_ test body orelse (← spanFrom start)

/-- Parse `while expr: block [else: block]`. -/
partial def parseWhile : ParserM Stmt := do
  let start ← currentSpan
  discard (expectKeyword .while_)
  let test ← parseExpression
  discard (expectDelimiter .colon)
  let body ← parseBlock
  let orelse ← if ← isKeyword .else_ then do
                  discard advance
                  discard (expectDelimiter .colon)
                  parseBlock
                else pure []
  return .while_ test body orelse (← spanFrom start)

/-- Parse `for target in expr: block [else: block]`. -/
partial def parseFor : ParserM Stmt := do
  let start ← currentSpan
  discard (expectKeyword .for_)
  let target ← parseTargetList
  discard (expectKeyword .in_)
  let iter ← parseExpressionList
  discard (expectDelimiter .colon)
  let body ← parseBlock
  let orelse ← if ← isKeyword .else_ then do
                  discard advance
                  discard (expectDelimiter .colon)
                  parseBlock
                else pure []
  return .for_ target iter body orelse (← spanFrom start)

/-- Parse `with item, item, ...: block`. -/
partial def parseWith : ParserM Stmt := do
  let start ← currentSpan
  discard (expectKeyword .with_)
  let items ← sepBy1 parseWithItem (discard (expectDelimiter .comma))
  discard (expectDelimiter .colon)
  let body ← parseBlock
  return .with_ items body (← spanFrom start)

/-- Parse a single with item: `expr [as target]`. -/
partial def parseWithItem : ParserM WithItem := do
  let ctx ← parseExpression
  let vars ← if ← isKeyword .as_ then do
                discard advance
                some <$> parseTargetList
              else pure none
  return { contextExpr := ctx, optionalVars := vars }

/-- Parse `try: block except ...: block [else: block] [finally: block]`. -/
partial def parseTry : ParserM Stmt := do
  let start ← currentSpan
  discard (expectKeyword .try_)
  discard (expectDelimiter .colon)
  let body ← parseBlock
  let mut handlers : List ExceptHandler := []
  while (← isKeyword .except_) do
    handlers := handlers ++ [← parseExceptHandler]
  let orelse ← if ← isKeyword .else_ then do
                  discard advance
                  discard (expectDelimiter .colon)
                  parseBlock
                else pure []
  let finally_ ← if ← isKeyword .finally_ then do
                    discard advance
                    discard (expectDelimiter .colon)
                    parseBlock
                  else pure []
  return .try_ body handlers orelse finally_ (← spanFrom start)

/-- Parse `except [Type [as name]]: block`. -/
partial def parseExceptHandler : ParserM ExceptHandler := do
  let start ← currentSpan
  discard (expectKeyword .except_)
  if ← isDelimiter .colon then
    discard advance
    let body ← parseBlock
    return ExceptHandler.mk none none body (← spanFrom start)
  if ← isOperator .star then discard advance
  let excType ← parseExpression
  let name ← if ← isKeyword .as_ then do
                discard advance
                let (n, _) ← parseName
                pure (some n)
              else pure none
  discard (expectDelimiter .colon)
  let body ← parseBlock
  return ExceptHandler.mk (some excType) name body (← spanFrom start)

/-- Parse function definition: `def name(params) [-> type]: block`. -/
partial def parseFunctionDef (isAsync : Bool) : ParserM Stmt := do
  let start ← currentSpan
  discard (expectKeyword .def_)
  let (name, _) ← parseName
  let args ← parseFuncParams
  let returns ← if ← isOperator .rarrow then do
                   discard advance
                   some <$> parseExpression
                 else pure none
  discard (expectDelimiter .colon)
  let body ← parseBlock
  let fd := FunctionDef.mk name args body [] returns (← spanFrom start)
  if isAsync then return .asyncFunctionDef fd
  return .functionDef fd

/-- Parse class definition: `class Name[(bases)]: block`. -/
partial def parseClassDef (_isAsync : Bool) : ParserM Stmt := do
  let start ← currentSpan
  discard (expectKeyword .class_)
  let (name, _) ← parseName
  let mut bases : List Expr := []
  let mut keywords : List CallKeyword := []
  if ← isDelimiter .lpar then
    discard advance
    if !(← isDelimiter .rpar) then
      let (b, kw) ← parseArgList
      bases := b
      keywords := kw
    discard (expectDelimiter .rpar)
  discard (expectDelimiter .colon)
  let body ← parseBlock
  return .classDef (ClassDef.mk name bases keywords body [] (← spanFrom start))

/-- Parse `async def/for/with`. -/
partial def parseAsync : ParserM Stmt := do
  discard (expectKeyword .async)
  match ← peekKind with
  | some (.keyword .def_) => parseFunctionDef true
  | some (.keyword .for_) =>
    let start ← currentSpan
    discard (expectKeyword .for_)
    let target ← parseTargetList
    discard (expectKeyword .in_)
    let iter ← parseExpressionList
    discard (expectDelimiter .colon)
    let body ← parseBlock
    let orelse ← if ← isKeyword .else_ then do
                    discard advance; discard (expectDelimiter .colon); parseBlock
                  else pure []
    return .asyncFor target iter body orelse (← spanFrom start)
  | some (.keyword .with_) =>
    let start ← currentSpan
    discard (expectKeyword .with_)
    let items ← sepBy1 parseWithItem (discard (expectDelimiter .comma))
    discard (expectDelimiter .colon)
    let body ← parseBlock
    return .asyncWith items body (← spanFrom start)
  | _ =>
    let tok ← match ← peek with
      | some t => pure t
      | none   => throw (.unexpectedEOF "expected def/for/with after async")
    throw (.expectedToken "def/for/with after async" tok.kind tok.span.start)

/-- Parse `@decorator` before def/class. -/
partial def parseDecorated : ParserM Stmt := do
  let mut decorators : List Expr := []
  while (← isOperator .at) do
    discard advance
    decorators := decorators ++ [← parseExpression]
    discard expectNewline
  match ← peekKind with
  | some (.keyword .def_) =>
    let stmt ← parseFunctionDef false
    return attachDecorators stmt decorators
  | some (.keyword .class_) =>
    let stmt ← parseClassDef false
    return attachDecorators stmt decorators
  | some (.keyword .async) =>
    discard advance
    let stmt ← parseFunctionDef true
    return attachDecorators stmt decorators
  | _ =>
    let tok ← match ← peek with
      | some t => pure t
      | none   => throw (.unexpectedEOF "expected def or class after decorator")
    throw (.expectedToken "def or class" tok.kind tok.span.start)

-- ## Match/case (structural pattern matching)

/-- Parse a match pattern: wildcard, class, value, or capture. -/
partial def parseMatchPattern : ParserM MatchPattern := do
  match ← peekKind with
  | some (.name "_") =>
    discard advance
    return .matchWildcard
  | some (.name n) =>
    discard advance
    -- Build dotted name chain for value patterns like Enum.VAL
    let mut expr := Expr.name n (← spanFrom (← currentSpan))
    let start ← currentSpan
    while (← isDelimiter .dot) do
      discard advance
      let (attr, _) ← parseName
      expr := Expr.attribute expr attr (← spanFrom start)
    -- Check if class pattern: Name(...) or Name.attr(...)
    if ← isDelimiter .lpar then
      discard advance
      let cls := expr
      let mut kwNames : List String := []
      let mut kwPats : List MatchPattern := []
      if !(← isDelimiter .rpar) then
        -- Parse keyword patterns: name=pattern, name=pattern, ...
        let mut first := true
        while true do
          if !first then
            if ← isDelimiter .comma then discard advance else break
          first := false
          if ← isDelimiter .rpar then break
          let (kwName, _) ← parseName
          discard (expectOperator .equal)
          let pat ← parseMatchPattern
          kwNames := kwNames ++ [kwName]
          kwPats := kwPats ++ [pat]
      discard (expectDelimiter .rpar)
      return .matchClass cls kwNames kwPats
    -- Bare name — capture pattern
    if !(← isDelimiter .dot) then
      match expr with
      | .name nm _ => return .matchCapture nm none
      | _ => pure ()
    -- Dotted name — value pattern
    return .matchValue expr
  | some (.integer _) | some (.float_ _) | some (.string _) =>
    let e ← parseExpression
    return .matchValue e
  | some (.keyword .none_) =>
    discard advance
    let sp ← spanFrom (← currentSpan)
    return .matchValue (.constant .none_ sp)
  | some (.keyword .true_) =>
    discard advance
    let sp ← spanFrom (← currentSpan)
    return .matchValue (.constant .true_ sp)
  | some (.keyword .false_) =>
    discard advance
    let sp ← spanFrom (← currentSpan)
    return .matchValue (.constant .false_ sp)
  | _ =>
    let tok ← match ← peek with
      | some t => pure t
      | none => throw (.unexpectedEOF "expected match pattern")
    throw (.expectedToken "match pattern" tok.kind tok.span.start)

/-- Parse a single `case pattern [if guard]: block`. -/
partial def parseCaseClause : ParserM MatchCase := do
  let start ← currentSpan
  -- Expect the soft keyword `case`
  match ← peekKind with
  | some (.name "case") => discard advance
  | _ =>
    let tok ← match ← peek with
      | some t => pure t
      | none => throw (.unexpectedEOF "expected 'case'")
    throw (.expectedToken "'case'" tok.kind tok.span.start)
  let pat ← parseMatchPattern
  let guard ← if ← isKeyword .if_ then do
                 discard advance
                 some <$> parseExpression
               else pure none
  discard (expectDelimiter .colon)
  let body ← parseBlock
  return MatchCase.mk pat guard body (← spanFrom start)

/-- Parse `match subject: NEWLINE INDENT case+ DEDENT`. -/
partial def parseMatchStmt : ParserM Stmt := do
  let start ← currentSpan
  -- Consume the soft keyword `match` (already verified as .name "match" by caller)
  discard advance
  let subject ← parseExpression
  discard (expectDelimiter .colon)
  discard expectNewline
  discard expectIndent
  let mut cases : List MatchCase := []
  while true do
    match ← peekKind with
    | some (.name "case") => cases := cases ++ [← parseCaseClause]
    | some .newline => discard advance
    | _ => break
  discard expectDedent
  return .match_ subject cases (← spanFrom start)

/-- Parse a module: statements until ENDMARKER. -/
partial def parseModule : ParserM Module := do
  let stmts ← parseStatements
  while (← isNewline) do discard advance
  return .module stmts

end -- mutual

end LeanPython.Parser
