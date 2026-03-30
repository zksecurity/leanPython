import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Stdlib.Json

open LeanPython.Runtime
open LeanPython.Interpreter

-- ============================================================
-- JSON intermediate AST
-- ============================================================

inductive JsonVal where
  | null
  | bool : Bool → JsonVal
  | num : Int → JsonVal
  | float : Float → JsonVal
  | str : String → JsonVal
  | arr : List JsonVal → JsonVal
  | obj : List (String × JsonVal) → JsonVal

-- ============================================================
-- JSON serialization (json.dumps)
-- ============================================================

/-- Escape a string for JSON output. -/
private partial def jsonEscapeString (s : String) : String := Id.run do
  let mut result := ""
  for c in s.toList do
    match c with
    | '"' => result := result ++ "\\\""
    | '\\' => result := result ++ "\\\\"
    | '\n' => result := result ++ "\\n"
    | '\t' => result := result ++ "\\t"
    | '\r' => result := result ++ "\\r"
    | c =>
      if c.toNat < 32 then
        let n := c.toNat
        let hexDigit (d : Nat) : Char :=
          if d < 10 then Char.ofNat (48 + d) else Char.ofNat (87 + d)
        result := result ++ "\\u"
          ++ String.ofList [hexDigit (n / 4096 % 16), hexDigit (n / 256 % 16),
                           hexDigit (n / 16 % 16), hexDigit (n % 16)]
      else
        result := result.push c
  return result

/-- Serialize a Python Value to a JSON string. -/
partial def jsonSerialize (v : Value) : InterpM String := do
  match v with
  | .none => return "null"
  | .bool true => return "true"
  | .bool false => return "false"
  | .int n => return toString n
  | .float f =>
    if f.isNaN then return "NaN"
    else if f.isInf then
      if f > 0 then return "Infinity" else return "-Infinity"
    else return (toString f)
  | .str s => return s!"\"{jsonEscapeString s}\""
  | .list ref => do
    let items ← heapGetList ref
    if items.isEmpty then return "[]"
    let mut parts : List String := []
    for item in items do
      parts := parts ++ [← jsonSerialize item]
    return "[" ++ ", ".intercalate parts ++ "]"
  | .tuple items => do
    if items.isEmpty then return "[]"
    let mut parts : List String := []
    for item in items do
      parts := parts ++ [← jsonSerialize item]
    return "[" ++ ", ".intercalate parts ++ "]"
  | .dict ref => do
    let pairs ← heapGetDict ref
    if pairs.isEmpty then return "{}"
    let mut parts : List String := []
    for (k, val) in pairs do
      let key ← jsonSerializeKey k
      let valStr ← jsonSerialize val
      parts := parts ++ [key ++ ": " ++ valStr]
    return "{" ++ ", ".intercalate parts ++ "}"
  | _ => throwTypeError s!"Object of type '{typeName v}' is not JSON serializable"
where
  jsonSerializeKey (k : Value) : InterpM String := do
    match k with
    | .str s => return s!"\"{jsonEscapeString s}\""
    | .int n => return s!"\"{n}\""
    | .bool true => return "\"true\""
    | .bool false => return "\"false\""
    | .none => return "\"null\""
    | _ => throwTypeError "keys must be str, int, float, bool or None"

/-- Python json.dumps(obj, indent=None, sort_keys=False) -/
partial def jsonDumps (args : List Value) (kwargs : List (String × Value))
    : InterpM Value := do
  -- ignore indent/sort_keys for simplicity, just serialize
  let _indent := kwargs.find? (fun p => p.1 == "indent")
  match args with
  | [obj] => do
    let result ← jsonSerialize obj
    return .str result
  | _ => throwTypeError "json.dumps() takes exactly 1 positional argument"

-- ============================================================
-- JSON parsing (json.loads)
-- ============================================================

private def skipWs (chars : List Char) : List Char :=
  chars.dropWhile (fun c => c == ' ' || c == '\n' || c == '\r' || c == '\t')

/-- Parse a JSON string literal (opening " already consumed). -/
private partial def parseJsonString (chars : List Char) (acc : List Char)
    : Except String (String × List Char) :=
  match chars with
  | [] => .error "Unterminated string"
  | '"' :: rest => .ok (String.ofList acc.reverse, rest)
  | '\\' :: esc :: rest =>
    if esc == 'u' then
      match rest with
      | h1 :: h2 :: h3 :: h4 :: rest' =>
        let parseHex (c : Char) : Nat :=
          if '0' ≤ c && c ≤ '9' then c.toNat - '0'.toNat
          else if 'a' ≤ c && c ≤ 'f' then c.toNat - 'a'.toNat + 10
          else if 'A' ≤ c && c ≤ 'F' then c.toNat - 'A'.toNat + 10
          else 0
        let cp := parseHex h1 * 4096 + parseHex h2 * 256 + parseHex h3 * 16 + parseHex h4
        parseJsonString rest' (Char.ofNat cp :: acc)
      | _ => .error "Invalid \\u escape"
    else
      let c := match esc with
        | '"' => '"' | '\\' => '\\' | '/' => '/' | 'n' => '\n'
        | 't' => '\t' | 'r' => '\r' | 'b' => Char.ofNat 8
        | 'f' => Char.ofNat 12 | other => other
      parseJsonString rest (c :: acc)
  | c :: rest => parseJsonString rest (c :: acc)

/-- Collect number characters. -/
private def collectNumberChars (chars : List Char) : List Char × List Char :=
  span chars
where
  span : List Char → List Char × List Char
    | c :: rest =>
      if c.isDigit || c == '-' || c == '+' || c == '.' || c == 'e' || c == 'E' then
        let (collected, remaining) := span rest
        (c :: collected, remaining)
      else ([], c :: rest)
    | [] => ([], [])

/-- Parse a JSON number. -/
private def parseJsonNumber (chars : List Char) : Except String (JsonVal × List Char) := do
  let (numChars, rest) := collectNumberChars chars
  let numStr := String.ofList numChars
  if numStr.isEmpty then .error "Expected number"
  let isFloat := numChars.any (fun c => c == '.' || c == 'e' || c == 'E')
  if isFloat then
    -- Simple float: split on '.'
    let parts := numStr.splitOn "."
    match parts with
    | [intPart, fracPart] =>
      let neg := intPart.startsWith "-"
      let absIntPart := if neg then String.ofList (intPart.toList.drop 1) else intPart
      let intVal := absIntPart.toNat?.getD 0
      let fracVal := fracPart.toNat?.getD 0
      let fracLen := fracPart.length
      let f := Float.ofNat intVal + Float.ofNat fracVal / Float.ofNat (10 ^ fracLen)
      .ok (.float (if neg then -f else f), rest)
    | [intPart] =>
      -- Could be "1e5" form
      match intPart.toInt? with
      | some n => .ok (.float (Float.ofInt n), rest)
      | none => .error s!"Cannot parse number: {numStr}"
    | _ => .error s!"Cannot parse number: {numStr}"
  else
    match numStr.toInt? with
    | some n => .ok (.num n, rest)
    | none => .error s!"Invalid number: {numStr}"

mutual
-- Parse a JSON value
partial def parseJsonValue (chars : List Char) : Except String (JsonVal × List Char) := do
  let cs := skipWs chars
  match cs with
  | [] => .error "Unexpected end of JSON"
  | '"' :: rest =>
    let (s, remaining) ← parseJsonString rest []
    .ok (.str s, remaining)
  | 't' :: 'r' :: 'u' :: 'e' :: rest => .ok (.bool true, rest)
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: rest => .ok (.bool false, rest)
  | 'n' :: 'u' :: 'l' :: 'l' :: rest => .ok (.null, rest)
  | '[' :: rest => parseJsonArray (skipWs rest) []
  | '{' :: rest => parseJsonObject (skipWs rest) []
  | c :: _ =>
    if c == '-' || c.isDigit then parseJsonNumber cs
    else .error s!"Unexpected character: '{c}'"

-- Parse a JSON array
partial def parseJsonArray (chars : List Char) (acc : List JsonVal)
    : Except String (JsonVal × List Char) := do
  let cs := skipWs chars
  match cs with
  | ']' :: rest => .ok (.arr acc.reverse, rest)
  | _ => do
    let (val, rest1) ← parseJsonValue cs
    let rest2 := skipWs rest1
    match rest2 with
    | ',' :: rest3 => parseJsonArray rest3 (val :: acc)
    | ']' :: rest3 => .ok (.arr (val :: acc).reverse, rest3)
    | _ => .error "Expected ',' or ']' in array"

-- Parse a JSON object
partial def parseJsonObject (chars : List Char) (acc : List (String × JsonVal))
    : Except String (JsonVal × List Char) := do
  let cs := skipWs chars
  match cs with
  | '}' :: rest => .ok (.obj acc.reverse, rest)
  | '"' :: rest => do
    let (key, rest1) ← parseJsonString rest []
    let rest2 := skipWs rest1
    match rest2 with
    | ':' :: rest3 => do
      let (val, rest4) ← parseJsonValue rest3
      let rest5 := skipWs rest4
      match rest5 with
      | ',' :: rest6 => parseJsonObject rest6 ((key, val) :: acc)
      | '}' :: rest6 => .ok (.obj ((key, val) :: acc).reverse, rest6)
      | _ => .error "Expected ',' or '}' in object"
    | _ => .error "Expected ':' after key in object"
  | _ => .error "Expected string key or '}' in object"
end  -- mutual

/-- Convert a parsed JsonVal to a Python Value, allocating heap objects. -/
private partial def jsonToValue (jv : JsonVal) : InterpM Value := do
  match jv with
  | .null => return .none
  | .bool b => return .bool b
  | .num n => return .int n
  | .float f => return .float f
  | .str s => return .str s
  | .arr items => do
    let mut elems : Array Value := #[]
    for item in items do
      elems := elems.push (← jsonToValue item)
    allocList elems
  | .obj pairs => do
    let mut kvs : Array (Value × Value) := #[]
    for (k, v) in pairs do
      kvs := kvs.push (.str k, ← jsonToValue v)
    allocDict kvs

/-- Python json.loads(s) -/
partial def jsonLoads (args : List Value) : InterpM Value := do
  match args with
  | [.str s] => do
    match parseJsonValue s.toList with
    | .ok (jv, _) => jsonToValue jv
    | .error e => throwValueError s!"json.decoder.JSONDecodeError: {e}"
  | _ => throwTypeError "json.loads() requires a string argument"

end LeanPython.Stdlib.Json
